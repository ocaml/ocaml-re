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
  @ List.map
      (fun (case : Capture_histories.case) ->
         { name = Capture_histories.name case
         ; pattern = case.pattern
         ; run = Capture_histories.run case
         })
      Capture_histories.cases
;;

(* Count the whole regex graph, so shared blocks are only counted once.
   Exact sizes are implementation snapshots for 64-bit native OCaml. *)
let reachable_words re = Obj.reachable_words (Obj.repr re)

let report ?compiled_words name re =
  let { Re.Stats.colors; states } = Re.stats re in
  (* Match the line-oriented snapshots in lib_test/expect/test_stats.ml. *)
  Printf.printf "%s:\n  colors: %d\n  states: %d\n" name colors states;
  Option.iter
    (fun compiled_words ->
       Printf.printf
         "  compiled_words: %d\n  forced_words: %d\n"
         compiled_words
         (reachable_words re))
    compiled_words
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
    20 zeroes:
      colors: 2
      states: 21
    lots of a's:
      colors: 3
      states: 5
    media type match:
      colors: 3
      states: 5
    uri:
      colors: 6
      states: 14
    tex gitignore:
      colors: 42
      states: 68
    http/manual/no group:
      colors: 13
      states: 557
    http/manual/group:
      colors: 13
      states: 40
    http/auto/execp no group:
      colors: 13
      states: 799
    http/auto/all_gen:
      colors: 13
      states: 66
    string traversal from #210:
      colors: 3
      states: 32
    kleene star compilation:
      colors: 2
      states: 3
    memory 1:
      colors: 3
      states: 1003
    memory 2:
      colors: 4
      states: 1003
    repeated sequence re:
      colors: 256
      states: 12801
    split on whitespace:
      colors: 2
      states: 4
    shared prefixes:
      colors: 27
      states: 6
    duplicate accepting states:
      colors: 256
      states: 2
    capture histories/adjacent:
      colors: 8
      states: 7
    capture histories/interleaved:
      colors: 5
      states: 5
    capture histories/nested/4:
      colors: 11
      states: 10
    capture histories/nested/16:
      colors: 12
      states: 25
    capture histories/log files:
      colors: 10
      states: 29
    capture histories/escape tokens:
      colors: 10
      states: 15
    capture histories/routes:
      colors: 21
      states: 56
    |}]
;;

(* Forcing a state builds all of its transitions eagerly, and the complete
   automaton of the [tex], non-capturing [http] and [memory] patterns is too
   large to construct. Only snapshot the patterns whose complete automaton is
   worth building, always on a copy: forcing mutates the automaton, and the
   other tests and benchmarks expect a cold regex.

   [expression IDs/narrow/8000000] is skipped as well: its complete automaton
   needs several GiB. *)
let force_benchmarks () =
  let case name =
    match List.find_opt (fun (c : case) -> String.equal c.name name) cases with
    | Some { name; pattern; _ } -> name, pattern
    | None -> failwith ("unknown benchmark case: " ^ name)
  in
  let forceable =
    List.map
      case
      [ "20 zeroes"
      ; "lots of a's"
      ; "media type match"
      ; "uri"
      ; "http/manual/group"
      ; "http/auto/all_gen"
      ; "string traversal from #210"
      ; "kleene star compilation"
      ; "repeated sequence re"
      ; "split on whitespace"
      ; "shared prefixes"
      ; "duplicate accepting states"
      ]
    @ List.map
        (fun (case : Capture_histories.case) -> Capture_histories.name case, case.pattern)
        Capture_histories.cases
    @ List.map
        (fun (branches, length) ->
           ( Id_patterns.broad_name branches length
           , Id_patterns.broad_then_narrow branches length () ))
        Id_patterns.broad_params
    @ List.map
        (fun length -> Id_patterns.narrow_name length, Id_patterns.narrow length ())
        (List.filter (fun length -> length < 8_000_000) Id_patterns.narrow_params)
  in
  List.iter
    (fun (name, pattern) ->
       let re = Re.compile pattern in
       let forced = Re.copy_re re in
       let compiled_words = reachable_words forced in
       Re.force_states forced;
       report ~compiled_words name forced)
    forceable
;;

(* Pin heap-size snapshots to the runtime they were recorded with. *)
let%test_module "fully forced benchmark automata" =
  (module struct
    let () =
      if String.equal Sys.ocaml_version "5.4.1"
      then
        let module Tests = struct
          let%expect_test "colors, states and reachable words" =
            force_benchmarks ();
            [%expect
              {|
              20 zeroes:
                colors: 2
                states: 23
                compiled_words: 491
                forced_words: 3856
              lots of a's:
                colors: 3
                states: 9
                compiled_words: 307
                forced_words: 901
              media type match:
                colors: 3
                states: 10
                compiled_words: 316
                forced_words: 1084
              uri:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 36756
              http/manual/group:
                colors: 13
                states: 374
                compiled_words: 838
                forced_words: 58100
              http/auto/all_gen:
                colors: 13
                states: 707
                compiled_words: 1277
                forced_words: 121883
              string traversal from #210:
                colors: 3
                states: 65
                compiled_words: 461
                forced_words: 7858
              kleene star compilation:
                colors: 2
                states: 5
                compiled_words: 270
                forced_words: 595
              repeated sequence re:
                colors: 256
                states: 12803
                compiled_words: 130325
                forced_words: 8906439
              split on whitespace:
                colors: 2
                states: 5
                compiled_words: 280
                forced_words: 646
              shared prefixes:
                colors: 27
                states: 8
                compiled_words: 6204
                forced_words: 11296
              duplicate accepting states:
                colors: 256
                states: 2
                compiled_words: 4985
                forced_words: 5485
              capture histories/adjacent:
                colors: 8
                states: 8
                compiled_words: 365
                forced_words: 912
              capture histories/interleaved:
                colors: 5
                states: 6
                compiled_words: 364
                forced_words: 756
              capture histories/nested/4:
                colors: 11
                states: 11
                compiled_words: 833
                forced_words: 1972
              capture histories/nested/16:
                colors: 12
                states: 26
                compiled_words: 2279
                forced_words: 7668
              capture histories/log files:
                colors: 10
                states: 30
                compiled_words: 1115
                forced_words: 4021
              capture histories/escape tokens:
                colors: 10
                states: 16
                compiled_words: 623
                forced_words: 2021
              capture histories/routes:
                colors: 21
                states: 65
                compiled_words: 1271
                forced_words: 8477
              expression IDs/broad/16/1024:
                colors: 9
                states: 1033
                compiled_words: 11053
                forced_words: 92585
              expression IDs/broad/4096/16384:
                colors: 17
                states: 16401
                compiled_words: 350486
                forced_words: 2293394
              expression IDs/broad/65536/262144:
                colors: 21
                states: 262165
                compiled_words: 5996822
                forced_words: 39815262
              expression IDs/narrow/1024:
                colors: 2
                states: 1027
                compiled_words: 10492
                forced_words: 74509
              expression IDs/narrow/1000000:
                colors: 2
                states: 1000003
                compiled_words: 10000252
                forced_words: 72024557
              |}]
          ;;
        end
        in
        ()
    ;;
  end)
;;
