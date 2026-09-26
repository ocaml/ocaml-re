open Import
(* Patterns shared by the benchmark executable and their stats snapshots.
   Every benched regex is defined here so that the two stay in sync. *)

let str_20_zeroes = String.make 20 '0'
let re_20_zeroes = Re.(str str_20_zeroes)

let lots_of_a's =
  String.init 101 (function
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

let all_bytes = String.init 256 Char.chr

(* Shared by the "string traversal from #210" benchmark. *)
let string_traversal_pattern = Re.Pcre.re "aaaaaaaaaaaaaaaaz"
let string_traversal_input = String.make (1000 * 1000) 'a'

(* Shared by the "kleene star compilation" benchmark. *)
let kleene_star_pattern = Re.rep (Re.char 'c')
let kleene_star_input = String.make 10_000 'c'

(* Shared by the "repeated sequence re" benchmark. *)
let repeated_sequence_pattern = Re.repn (Re.str all_bytes) 50 (Some 50)
let repeated_sequence_input = String.concat "" (List.init 50 (fun _ -> all_bytes))

(* Shared by the "split on whitespace" benchmark. *)
let split_input =
  let s = Bytes.make 1_000 '_' in
  for i = 0 to 100 do
    Bytes.set s (i * 9) ' '
  done;
  Bytes.to_string s
;;

let split_pattern = Re.rep1 Re.space

(* Shared by the "shared prefixes" benchmark. This regular expression can be
   heavily optimized by computing the shared prefix. *)
let make_ext =
  let chars = "abcdefghiklmnopqrstuvwxyz" in
  let buf = Buffer.create 4 in
  let rec loop remains =
    match remains with
    | 0 -> Buffer.contents buf
    | _ ->
      let char = remains mod String.length chars in
      Buffer.add_char buf chars.[char];
      loop (remains / String.length chars)
  in
  fun n ->
    Buffer.clear buf;
    loop n
;;

let prefix_base = String.make 20 'x' ^ "."
let prefix_extensions = List.init 100 make_ext
let prefix_inputs = List.map (fun ext -> prefix_base ^ ext) prefix_extensions

let prefixes_pattern =
  List.map (fun ext -> Re.(seq [ rep1 any; char '.'; str ext ])) prefix_extensions
  |> Re.alt
;;

(* Shared by the "duplicate accepting states" benchmark. The empty captures
   always win. The shadowed literal keeps every byte in a distinct color, so
   the word-byte inputs learn different transitions to the same accepting
   state. Eager status computation can build capture metadata for candidates
   that the state interner then discards. *)
let duplicate_pattern =
  Re.alt [ Re.seq (List.init 4 (fun _ -> Re.group Re.epsilon)); Re.str all_bytes ]
;;

let duplicate_inputs =
  let wordc = Re.compile Re.wordc in
  List.init 256 (fun byte -> String.make 1 (Char.chr byte))
  |> List.filter (Re.execp wordc)
;;

type case =
  { name : string
  ; pattern : Re.t
  ; run : Re.re -> unit
  }

let cases =
  let raw =
    List.map
      (fun (name, pattern, inputs) ->
         { name
         ; pattern
         ; run = (fun re -> List.iter (fun s -> ignore (Re.execp re s : bool)) inputs)
         })
      benchmarks
  in
  let tex =
    [ { name = "tex gitignore"
      ; pattern = Tex.ignore_re
      ; run =
          (fun re ->
            List.iter (fun s -> ignore (Re.execp re s : bool)) Tex.ignore_filesnames)
      }
    ]
  in
  let http =
    let open Http.Export in
    [ { name = "http/manual/no group"
      ; pattern = request
      ; run = (fun re -> Http.read_all 0 re Http.requests)
      }
    ; { name = "http/manual/group"
      ; pattern = request_g
      ; run = (fun re -> Http.read_all 0 re Http.requests)
      }
    ; { name = "http/auto/execp no group"
      ; pattern = requests
      ; run = (fun re -> ignore (Re.execp re Http.requests : bool))
      }
    ; { name = "http/auto/all_gen"
      ; pattern = requests_g
      ; run = (fun re -> ignore (Re.all re Http.requests))
      }
    ]
  in
  let memory_run re =
    let str = Memory_patterns.str in
    ignore (Re.execp ~pos:0 ~len:(String.length str) re str : bool)
  in
  raw
  @ tex
  @ http
  @ [ { name = "string traversal from #210"
      ; pattern = string_traversal_pattern
      ; run = (fun re -> ignore (Re.execp ~pos:0 re string_traversal_input : bool))
      }
    ; { name = "kleene star compilation"
      ; pattern = kleene_star_pattern
      ; run = (fun re -> ignore (Re.execp re kleene_star_input : bool))
      }
    ; { name = "memory 1"; pattern = Memory_patterns.re (); run = memory_run }
    ; { name = "memory 2"; pattern = Memory_patterns.re2 (); run = memory_run }
    ; { name = "repeated sequence re"
      ; pattern = repeated_sequence_pattern
      ; run = (fun re -> ignore (Re.execp re repeated_sequence_input : bool))
      }
    ; { name = "split on whitespace"
      ; pattern = split_pattern
      ; run = (fun re -> ignore (Re.split_full re split_input))
      }
    ; { name = "shared prefixes"
      ; pattern = prefixes_pattern
      ; run = (fun re -> List.iter (fun s -> ignore (Re.execp re s : bool)) prefix_inputs)
      }
    ; { name = "duplicate accepting states"
      ; pattern = duplicate_pattern
      ; run =
          (fun re -> List.iter (fun input -> ignore (Re.exec re input)) duplicate_inputs)
      }
    ]
;;

let report name re =
  let { Re.Stats.colors; states } = Re.stats re in
  Printf.printf "%s colors=%d states=%d\n" name colors states
;;

(* States are interned lazily. Each snapshot compiles a fresh regex and
   executes the same inputs as the benchmark before inspecting it. *)
let%expect_test "benchmark automaton colors and states" =
  List.iter
    (fun { name; pattern; run } ->
       let re = Re.compile pattern in
       run re;
       report name re)
    cases;
  [%expect
    {|
    20 zeroes colors=2 states=21
    lots of a's colors=3 states=5
    media type match colors=3 states=5
    uri colors=5 states=15
    tex gitignore colors=42 states=68
    http/manual/no group colors=13 states=648
    http/manual/group colors=13 states=45
    http/auto/execp no group colors=13 states=868
    http/auto/all_gen colors=13 states=71
    string traversal from #210 colors=3 states=32
    kleene star compilation colors=2 states=3
    memory 1 colors=3 states=1003
    memory 2 colors=4 states=1003
    repeated sequence re colors=256 states=12801
    split on whitespace colors=2 states=5
    shared prefixes colors=27 states=6
    duplicate accepting states colors=256 states=2
    |}]
;;
