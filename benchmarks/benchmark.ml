open Core
open Core_bench

let str_20_zeroes = String.make 20 '0'
let re_20_zeroes = Re.(str str_20_zeroes)

let lots_of_a's =
  String.init 101 ~f:(function
    | 100 -> 'b'
    | _ -> 'a')
;;

let lots_o_a's_re = Re.(seq [ char 'a'; opt (char 'a'); char 'b' ])

let media_type_re =
  let re = Re.Emacs.re ~case:true "[ \t]*\\([^ \t;]+\\)" in
  Re.(seq [ start; re ])
;;

(* Taken from https://github.com/rgrinberg/ocaml-uri/blob/903ef1010f9808d6f3f6d9c1fe4b4eabbd76082d/lib/uri.ml*)
let uri_reference =
  Re.Posix.re "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
;;

let uris =
  [ "https://google.com"
  ; "http://yahoo.com/xxx/yyy?query=param&one=two"
  ; "file:/random_crap"
  ]
;;

let benchmarks =
  [ "20 zeroes", re_20_zeroes, [ str_20_zeroes ]
  ; "lots of a's", lots_o_a's_re, [ lots_of_a's ]
  ; "media type match", media_type_re, [ " foo/bar ; charset=UTF-8" ]
  ; "uri", uri_reference, uris
  ]
;;

let test ~name re f =
  [ Bench.Test.create ~name (fun () -> f re)
  ; (let re () =
       let re = lazy (re ()) in
       Lazy.force re
     in
     Bench.Test.create ~name:(sprintf "%s (compiled)" name) (fun () -> f re))
  ]
;;

let exec_bench exec name (re : Re.t) cases =
  Bench.Test.create_group
    ~name
    (List.concat_map cases ~f:(fun data ->
       let name =
         let len = String.length data in
         if len > 70
         then Printf.sprintf "%s .. (%d)" (String.sub data ~pos:0 ~len:10) len
         else data
       in
       let re () = Re.compile re in
       test ~name re (fun re -> ignore (exec (re ()) data))))
;;

let exec_bench_many exec name re cases =
  test
    ~name
    (fun () -> Re.compile re)
    (fun re ->
      let re = re () in
      List.iter cases ~f:(fun x -> ignore (exec re x)))
;;

let string_traversal =
  let len = 1000 * 1000 in
  let s = String.make len 'a' in
  let re =
    let re = Re.Pcre.re "aaaaaaaaaaaaaaaaz" in
    fun () -> Re.compile re
  in
  test ~name:"string traversal from #210" re (fun re ->
    ignore (Re.execp (re ()) s ~pos:0))
;;

let compile_clean_star =
  let c = 'c' in
  let s = String.make 10_000 c in
  let re = Re.rep (Re.char 'c') in
  let re () = Re.compile re in
  test ~name:"kleene star compilation" re (fun re -> ignore (Re.execp (re ()) s))
;;

let benchmarks =
  let benches =
    List.map benchmarks ~f:(fun (name, re, cases) ->
      Bench.Test.create_group
        ~name
        [ exec_bench Re.exec "exec" re cases
        ; exec_bench Re.execp "execp" re cases
        ; exec_bench Re.exec_opt "exec_opt" re cases
        ])
  in
  let http_benches =
    let open Http.Export in
    let manual =
      [ request, "no group"; request_g, "group" ]
      |> List.concat_map ~f:(fun (re, name) ->
        let re () = Re.compile re in
        test ~name re (fun re ->
          let re = re () in
          Http.read_all 0 re Http.requests))
      |> Bench.Test.create_group ~name:"manual"
    in
    let many =
      [ test
          ~name:"execp no group"
          (fun () -> Re.compile requests)
          (fun re -> ignore (Re.execp (re ()) Http.requests))
      ; test
          ~name:"all_gen"
          (fun () -> Re.compile requests_g)
          (fun re -> Http.requests |> Re.all (re ()))
      ]
      |> List.concat
      |> Bench.Test.create_group ~name:"auto"
    in
    Bench.Test.create_group ~name:"http" [ manual; many ]
  in
  benches
  @ [ [ exec_bench_many Re.execp "execp"; exec_bench_many Re.exec_opt "exec_opt" ]
      |> List.concat_map ~f:(fun f -> f Tex.ignore_re Tex.ignore_filesnames)
      |> Bench.Test.create_group ~name:"tex gitignore"
    ]
  @ [ http_benches ]
  @ string_traversal
  @ compile_clean_star
  @ Memory.benchmarks
;;

let () =
  let benchmarks =
    match Sys.getenv "RE_BENCH_FILTER" with
    | None -> benchmarks
    | Some only ->
      let only = String.split ~on:',' only in
      let filtered =
        List.filter benchmarks ~f:(fun bench ->
          let name = Bench.Test.name bench in
          List.mem only name ~equal:String.equal)
      in
      (match filtered with
       | _ :: _ -> filtered
       | [] ->
         print_endline "No benchmarks to run. Your options are:";
         List.iter benchmarks ~f:(fun bench ->
           let name = Bench.Test.name bench in
           Printf.printf "- %s\n" name);
         exit 1)
  in
  Command_unix.run (Bench.make_command benchmarks)
;;
