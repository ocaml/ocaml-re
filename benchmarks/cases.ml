open Import
open Workload

let all_bytes = String.init 256 Char.chr

(* Word-byte inputs learn different transitions to the same accepting state
   in the duplicate-status matrix. *)
let duplicate_inputs =
  let wordc = Re.compile Re.wordc in
  List.init 256 (fun byte -> String.make 1 (Char.chr byte))
  |> List.filter (Re.execp wordc)
;;

let check_http re =
  (* The historical header key accepts newlines, so even the single-request
     pattern consumes this entire corpus in one match. Preserve that workload. *)
  match Re.all re Http.requests with
  | [ group ] -> assert (Re.Group.offset group 0 = (0, String.length Http.requests))
  | _ -> failwith "HTTP workload: expected one match covering the corpus"
;;

(* From ocaml-uri, 903ef1010f9808d6f3f6d9c1fe4b4eabbd76082d/lib/uri.ml. *)
let uri_pattern () =
  Re.Posix.re "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
;;

let uri_inputs =
  [ "https://google.com"
  ; "http://yahoo.com/xxx/yyy?query=param&one=two"
  ; "file:/random_crap"
  ]
;;

let cases =
  [ inputs "20 zeroes" (fun () ->
      let s = String.make 20 '0' in
      Re.str s, [ yes s ])
  ; inputs "lots of a's" (fun () ->
      Re.(seq [ char 'a'; opt (char 'a'); char 'b' ]), [ yes (String.make 100 'a' ^ "b") ])
  ; inputs "media type match" (fun () ->
      let re = Re.Emacs.re ~case:true "[ \t]*\\([^ \t;]+\\)" in
      Re.(seq [ start; re ]), [ yes " foo/bar ; charset=UTF-8" ])
  ; inputs "uri" (fun () -> uri_pattern (), List.map yes uri_inputs)
  ; inputs ~force:(`Inputs "exponential complete automaton") "tex gitignore" (fun () ->
      Tex.ignore_re, Tex.samples)
  ; custom
      ~force:(`Inputs "exponential complete automaton")
      "http/manual/no group"
      (fun () ->
         Http.Export.request, (fun re -> Http.read_all 0 re Http.requests), check_http)
  ; custom "http/manual/group" (fun () ->
      Http.Export.request_g, (fun re -> Http.read_all 0 re Http.requests), check_http)
  ; custom
      ~force:(`Inputs "exponential complete automaton")
      "http/auto/execp no group"
      (fun () ->
         ( Http.Export.requests
         , (fun re -> ignore (Re.execp re Http.requests : bool))
         , check_http ))
  ; custom "http/auto/all_gen" (fun () ->
      Http.Export.requests_g, (fun re -> ignore (Re.all re Http.requests)), check_http)
  ; inputs "string traversal from #210" (fun () ->
      Re.Pcre.re "aaaaaaaaaaaaaaaaz", [ no (String.make (1000 * 1000) 'a') ])
  ; inputs "kleene star compilation" (fun () ->
      Re.rep (Re.char 'c'), [ yes (String.make 10_000 'c') ])
  ; inputs ~force:(`Inputs "exponential complete automaton") "memory 1" (fun () ->
      Memory_patterns.re (), [ yes Memory_patterns.str ])
  ; inputs ~force:(`Inputs "exponential complete automaton") "memory 2" (fun () ->
      Memory_patterns.re2 (), [ no Memory_patterns.str ])
  ; inputs "repeated sequence re" (fun () ->
      Re.repn (Re.str all_bytes) 50 (Some 50), [ yes (repeat 50 all_bytes) ])
  ; custom "split on whitespace" (fun () ->
      let s = Bytes.make 1_000 '_' in
      for i = 0 to 100 do
        Bytes.set s (i * 9) ' '
      done;
      let input = Bytes.to_string s in
      let run re = ignore (Re.split_full re input) in
      let check re =
        let actual =
          Re.split_full re input
          |> List.map (function
            | `Text s -> `Text s
            | `Delim g -> `Delim (Re.Group.get g 0))
        in
        let expected =
          List.concat_map
            (fun i -> [ `Delim " "; `Text (String.make (if i = 100 then 99 else 8) '_') ])
            (List.init 101 Fun.id)
        in
        assert (actual = expected)
      in
      Re.rep1 Re.space, run, check)
  ; inputs "shared prefixes" (fun () ->
      let make_ext n =
        let chars = "abcdefghiklmnopqrstuvwxyz" in
        let buf = Buffer.create 4 in
        let rec loop = function
          | 0 -> Buffer.contents buf
          | n ->
            Buffer.add_char buf chars.[n mod String.length chars];
            loop (n / String.length chars)
        in
        loop n
      in
      let extensions = List.init 100 make_ext in
      let base = String.make 20 'x' ^ "." in
      let pattern =
        List.map (fun ext -> Re.(seq [ rep1 any; char '.'; str ext ])) extensions
        |> Re.alt
      in
      pattern, List.map (fun ext -> yes (base ^ ext)) extensions)
  ]
  @ List.mapi
      (fun i input ->
         inputs
           (Printf.sprintf "uri/input/%d" (i + 1))
           (fun () -> uri_pattern (), [ yes input ]))
      uri_inputs
  @ List.map
      (fun (c : Pattern.case) ->
         let force =
           if c.forceable then `Full else `Inputs "impractical complete automaton"
         in
         inputs ~force c.name (fun () -> c.pattern, List.map yes c.yes @ List.map no c.no))
      (Common_patterns.cases @ Edge_patterns.cases)
  @ List.map
      (fun (c : Capture_histories.case) ->
         custom (Capture_histories.name c) (fun () ->
           c.pattern, Capture_histories.run c, Capture_histories.check c))
      Capture_histories.cases
  @ List.concat_map
      (fun (name, pattern) ->
         List.map
           (fun len ->
              inputs
                ~force:(`Inputs "exponential complete automaton")
                (Printf.sprintf "%s/%d" name len)
                (fun () -> pattern (), [ no (String.sub Memory_patterns.str 0 len) ]))
           [ 10; 20; 40; 80; 100; Memory_patterns.size ])
      [ "memory 1", Memory_patterns.re; "memory 2", Memory_patterns.re2 ]
  @ List.map
      (fun (branches, length) ->
         generated
           (Id_patterns.broad_name branches length)
           ~pattern:(Id_patterns.broad_then_narrow branches length)
           ~samples:(fun () -> [ yes ("az" ^ String.make length '0') ]))
      Id_patterns.broad_params
  @ List.map
      (fun length ->
         (* The largest case needs several GiB even without complete forcing. *)
         generated
           ~runtest:(length < 8_000_000)
           (Id_patterns.narrow_name length)
           ~pattern:(Id_patterns.narrow length)
           ~samples:(fun () -> [ yes (String.make length '0') ]))
      Id_patterns.narrow_params
;;
