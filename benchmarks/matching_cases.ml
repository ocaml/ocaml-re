open Import
open Workload

let prose () =
  String.concat " " (List.init 2048 (fun _ -> "the quick brown fox jumps over lazy dogs"))
;;

let long_a () = String.make 65536 'a'

let boolean =
  let email = "\\A[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}\\z" in
  List.map
    (fun (name, make) ->
       inputs ("boolean/" ^ name) (fun () ->
         let pattern, sample = make () in
         Re.no_group pattern, [ sample ]))
    Re.
      [ ("literal-short-hit", fun () -> str "needle", yes "a needle in a haystack")
      ; ("literal-long-miss", fun () -> str "needle", no (prose ()))
      ; ("literal-long-late", fun () -> str "needle", yes (prose () ^ "needle"))
      ; ("literal-overlap-miss", fun () -> str "aaaaaaaaaaaaaaaaz", no (long_a ()))
      ; ( "alt-long-miss"
        , fun () -> alt [ str "ERROR"; str "WARN"; str "FATAL" ], no (prose ()) )
      ; ("suffix-miss", fun () -> seq [ str ".ml"; eos ], no (prose ()))
      ; ("star-greedy-hit", fun () -> rep (char 'a'), yes (long_a ()))
      ; ("plus-greedy-hit", fun () -> rep1 (char 'a'), yes (long_a ()))
      ; ("anchored-repeat", fun () -> whole_string (rep1 (char 'a')), yes (long_a ()))
      ; ("anchored-class", fun () -> whole_string (rep1 (rg 'a' 'z')), yes (long_a ()))
      ; ( "anchored-class-varied"
        , fun () ->
            ( whole_string (rep1 (rg 'a' 'z'))
            , yes (String.init 65536 (fun i -> Char.chr (97 + (i mod 26)))) ) )
      ; ( "anchored-class-fail"
        , fun () -> whole_string (rep1 (rg 'a' 'z')), no (long_a () ^ "!") )
      ; ("email-hit", fun () -> Perl.re email, yes "alice.smith42@example.co.uk")
      ; ("email-miss", fun () -> Perl.re email, no "alice.smith42!example.co.uk")
      ; ( "http-hit"
        , fun () ->
            ( Perl.re "\\A(?:GET|POST|PUT|DELETE) /[^ ]* HTTP/1\\.[01]\\r\\n"
            , yes "GET /some/path?q=123 HTTP/1.1\r\n" ) )
      ; ("word-boundary-miss", fun () -> word (str "needle"), no (prose ()))
      ; ("line-anchor-miss", fun () -> seq [ bol; str "ERROR"; eol ], no (prose ()))
      ; ( "longest-hit"
        , fun () ->
            longest (alt [ char 'a'; seq [ rep1 (char 'a'); char 'b' ] ]), yes (long_a ())
        )
      ; ("ambiguous-miss", fun () -> Perl.re "(?:a|aa)*b", no (long_a ()))
      ; ("short-reject", fun () -> whole_string (str "abcdef"), no "xyz")
      ; ("short-accept", fun () -> whole_string (str "abcdef"), yes "abcdef")
      ; ( "literal-dense-candidates"
        , fun () -> str ("b" ^ String.make 255 'a'), no (long_a ()) )
      ; ("literal-dense-short", fun () -> str "baaaaaaa", no (long_a ()))
      ; ( "cycle-anchored"
        , fun () -> whole_string (rep1 (str "ab")), yes (repeat 32768 "ab") )
      ; ( "class-late-hit"
        , fun () -> seq [ rep1 (rg 'a' 'z'); char '!' ], yes (long_a () ^ "!") )
      ; ( "word-boundary-late-hit"
        , fun () -> word (str "needle"), yes (prose () ^ " needle ") )
      ; ("literal-1k-miss", fun () -> str "needle", no (String.sub (prose ()) 0 1024))
      ; ("literal-1m-miss", fun () -> str "needle", no (String.make 1048576 'a'))
      ; ( "path-glob"
        , fun () ->
            ( Glob.glob ~anchored:true ~expand_braces:true "**/*.{ml,mli}"
            , yes "src/some/directory/parser.mli" ) )
      ]
;;

let histories =
  List.concat_map
    (fun (name, make) ->
       List.map
         (fun no_group ->
            inputs
              ("histories/" ^ name ^ if no_group then "/no group" else "/captures")
              (fun () ->
                 let pattern, samples = make () in
                 (if no_group then Re.no_group pattern else pattern), samples))
         [ false; true ])
    Re.
      [ ( "literal"
        , fun () ->
            str "needle", [ no "none"; yes "a needle here"; yes "needle"; no "need!" ] )
      ; ( "overlap"
        , fun () ->
            ( str (String.make 31 'a' ^ "b")
            , [ no (String.make 512 'a'); yes (String.make 512 'a' ^ "b"); no "!" ] ) )
      ; ( "email"
        , fun () ->
            ( Perl.re "\\A([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+)\\.([a-zA-Z]{2,})\\z"
            , [ yes "alice.smith42@example.co.uk"
              ; no "alice!example.co.uk"
              ; yes "a@b.org"
              ; no ""
              ] ) )
      ; ( "http"
        , fun () ->
            ( Perl.re "\\A(GET|POST|PUT|DELETE) (/[^ ]*) HTTP/(1\\.[01])\\r\\n"
            , [ yes "GET /some/path?q=123 HTTP/1.1\r\n"
              ; yes "POST / HTTP/1.0\r\n"
              ; no "HEAD / HTTP/1.1\r\n"
              ; no "bad"
              ] ) )
      ; ( "ambiguous"
        , fun () ->
            ( Perl.re "((a|aa)*)(b)"
            , [ no (String.make 128 'a')
              ; yes (String.make 128 'a' ^ "b")
              ; yes "ab"
              ; no "!"
              ] ) )
      ; ( "boundaries"
        , fun () ->
            ( group (word (str "needle"))
            , [ yes "needle"
              ; yes "!needle!"
              ; yes "a needle\n"
              ; no "aneedle"
              ; no "needle\255"
              ] ) )
      ; ( "many-groups"
        , fun () ->
            ( whole_string (seq (List.init 32 (fun _ -> group (opt (char 'a')))))
            , List.init 34 (fun n -> String.make n 'a', n <= 32) ) )
      ; ( "many-marks"
        , fun () ->
            ( whole_string (seq (List.init 32 (fun _ -> snd (mark (opt (char 'a'))))))
            , List.init 34 (fun n -> String.make n 'a', n <= 32) ) )
      ]
;;

let first =
  List.map
    (fun (name, make) ->
       inputs ("matching/" ^ name) (fun () ->
         let pattern, strings = make () in
         pattern, List.map yes strings))
    Re.
      [ ("literal", fun () -> str "abcdefgh", [ "xxabcdefgh" ])
      ; ( "greedy"
        , fun () -> seq [ char 'a'; group (rep any); char 'b' ], [ "xaxxxxxxxxxbxx" ] )
      ; ( "lazy"
        , fun () ->
            seq [ char 'a'; non_greedy (group (rep any)); char 'b' ], [ "xaxxxxxxxxxbxx" ]
        )
      ; ( "captures"
        , fun () ->
            ( whole_string (rep (nest (alt [ group (str "ab"); group (str "ac") ])))
            , [ repeat 30 "abac" ] ) )
      ; ( "word boundaries"
        , fun () -> seq [ bow; group (rep1 wordc); eow ], [ "! hello world!" ] )
      ; ( "broad alternatives"
        , fun () ->
            ( alt
                (List.init 32 (fun i ->
                   group (seq [ rep (char 'a'); str (string_of_int i) ])))
            , [ "aaaa31" ] ) )
      ; ( "fixed repetition"
        , fun () -> whole_string (repn (set "ab") 128 (Some 128)), [ String.make 128 'a' ]
        )
      ; ("longest", fun () -> longest (seq [ group (rep any); char 'b' ]), [ "aaaaab" ])
      ; ("nullable", fun () -> rep (group (opt (char 'a'))), [ "aaaaab" ])
      ; ( "nested erasure"
        , fun () ->
            ( whole_string
                (rep1 (nest (group (seq [ opt (group (char 'a')); group (char 'b') ]))))
            , [ "b"; "ab"; "abb"; "abab"; "bb" ] ) )
      ; ( "capture loop"
        , fun () -> Perl.re "^((a|b)+)(c*)$", [ "ab"; "aabbc"; "bccc"; "aba" ] )
      ; ("unanchored bytes", fun () -> str Cases.all_bytes, [ Cases.all_bytes ])
      ; ( "sequential32"
        , fun () ->
            ( whole_string (seq (List.init 32 (fun _ -> group (char 'a'))))
            , [ String.make 32 'a' ] ) )
      ; ( "promotion email"
        , fun () ->
            ( Perl.re "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,}"
            , [ "some.body+tag@example.org" ] ) )
      ; ( "promotion http"
        , fun () ->
            ( Perl.re "^(GET|POST|HEAD) +[^ ]+ +HTTP/[0-9]\\.[0-9]\\r?\\n"
            , [ "GET /index.html HTTP/1.1\r\n" ] ) )
      ]
;;

let cases =
  boolean
  @ histories
  @ first
  @ [ inputs ~force:(`Inputs "exponential complete automaton") "boolean/tex" (fun () ->
        Re.no_group Tex.ignore_re, Tex.samples)
    ]
;;
