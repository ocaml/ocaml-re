module Re = Re_private.Re

let matches parse pattern input =
  Re.execp (Re.compile (Re.whole_string (parse pattern))) input
;;

let%expect_test "all named classes and complements classify every byte" =
  let between lo hi x = lo <= x && x <= hi in
  let upper x = between 65 90 x || between 192 214 x || between 216 222 x in
  let lower x = between 97 122 x || x = 181 || between 223 246 x || between 248 255 x in
  let alpha x = upper x || lower x || x = 170 || x = 186 in
  let digit = between 48 57 in
  let alnum x = alpha x || digit x in
  let graph x = between 33 126 x || between 160 255 x in
  let classes =
    [ "alpha", alpha
    ; "alnum", alnum
    ; "ascii", between 0 127
    ; ("blank", fun x -> x = 9 || x = 32)
    ; ("cntrl", fun x -> between 0 31 x || between 127 159 x)
    ; "digit", digit
    ; "lower", lower
    ; ("print", fun x -> x = 32 || graph x)
    ; ("space", fun x -> between 9 13 x || x = 32)
    ; "upper", upper
    ; ("word", fun x -> alnum x || x = 95)
    ; ("punct", fun x -> graph x && not (alnum x))
    ; "graph", graph
    ; ("xdigit", fun x -> digit x || between 65 70 x || between 97 102 x)
    ]
  in
  List.iter
    (fun (parser, parse) ->
       List.iter
         (fun (name, predicate) ->
            List.iter
              (fun complement ->
                 let pattern = "[[:" ^ (if complement then "^" else "") ^ name ^ ":]]" in
                 let re = Re.compile (Re.whole_string (parse pattern)) in
                 for byte = 0 to 255 do
                   let actual = Re.execp re (String.make 1 (Char.chr byte)) in
                   let expected = predicate byte <> complement in
                   if actual <> expected
                   then failwith (Printf.sprintf "%s %s byte=%d" parser pattern byte)
                 done;
                 assert (not (Re.execp re ""));
                 assert (not (Re.execp re "aa")))
              [ false; true ])
         classes)
    [ "Perl", Re.Perl.re ~opts:[]; "Posix", Re.Posix.re ~opts:[] ];
  [%expect {| |}]
;;

let%expect_test "Perl escape bytes and malformed syntax have precise outcomes" =
  List.iter
    (fun (pattern, byte) ->
       let re = Re.compile (Re.whole_string (Re.Perl.re pattern)) in
       for i = 0 to 255 do
         assert (Re.execp re (String.make 1 (Char.chr i)) = (i = byte))
       done)
    [ {|\e|}, 27
    ; {|\f|}, 12
    ; {|\n|}, 10
    ; {|\r|}, 13
    ; {|\t|}, 9
    ; {|\x41|}, 65
    ; {|\x1|}, 1
    ; {|\xAF|}, 175
    ; {|\x{f}|}, 15
    ; {|\000|}, 0
    ; {|\377|}, 255
    ; {|[\1]|}, 1
    ];
  List.iter
    (fun (pattern, expected) ->
       match Re.Perl.re_result pattern with
       | Error actual ->
         if actual <> expected then failwith ("wrong Perl error: " ^ pattern)
       | Ok _ -> failwith ("Perl accepted: " ^ pattern))
    [ {|\E|}, `Parse_error
    ; {|\k|}, `Parse_error
    ; {|\xGG|}, `Parse_error
    ; {|\x|}, `Parse_error
    ; {|\x{}|}, `Parse_error
    ; {|\x{123}|}, `Parse_error
    ; {|\o|}, `Parse_error
    ; {|\o{8}|}, `Parse_error
    ; {|\777|}, `Parse_error
    ; {|\|}, `Parse_error
    ; {|[\q]|}, `Parse_error
    ; {|[\|}, `Parse_error
    ; {|[|}, `Parse_error
    ; {|(?=a)|}, `Parse_error
    ; {|(?<|}, `Parse_error
    ; {|(?<name|}, `Parse_error
    ; {|(?<1>x)|}, `Parse_error
    ; {|(?<na!me>x)|}, `Parse_error
    ; {|)|}, `Parse_error
    ; {|*a|}, `Parse_error
    ; {|+a|}, `Parse_error
    ; {|?a|}, `Parse_error
    ; {|a{}|}, `Parse_error
    ; {|a{2|}, `Parse_error
    ; {|a{3,2}|}, `Parse_error
    ; {|[[.ab.]]|}, `Not_supported
    ; {|[[.|}, `Parse_error
    ; {|[[.a.|}, `Parse_error
    ; {|[[:unknown:]]|}, `Parse_error
    ; {|[[:digit|}, `Parse_error
    ; {|[[:digit:x]|}, `Parse_error
    ];
  assert (matches (Re.Perl.re ~opts:[]) "[[.a.]]" "a");
  assert (matches (Re.Perl.re ~opts:[]) "[[]" "[");
  [%expect {| |}]
;;

let%expect_test "POSIX quantifiers, literal escapes and parser errors" =
  List.iter
    (fun (pattern, yes, no) ->
       assert (matches (Re.Posix.re ~opts:[]) pattern yes);
       assert (not (matches (Re.Posix.re ~opts:[]) pattern no)))
    [ "a*", "", "b"
    ; "a?", "a", "aa"
    ; "a{2}", "aa", "a"
    ; "a{1,3}", "aaa", "aaaa"
    ; "a{2,}", "aaaa", "a"
    ; "[a-]", "-", "b"
    ; "[a-[:digit:]]", "3", "z"
    ; "[[.a.]]", "a", "b"
    ; "[[]", "[", "]"
    ];
  String.iter
    (fun c ->
       assert (matches (Re.Posix.re ~opts:[]) ("\\" ^ String.make 1 c) (String.make 1 c)))
    "|()*+?[.^${\\";
  List.iter
    (fun (pattern, expected) ->
       match Re.Posix.re_result pattern with
       | Error actual -> assert (actual = expected)
       | Ok _ -> failwith ("Posix accepted: " ^ pattern))
    [ {|\q|}, `Parse_error
    ; {|\|}, `Parse_error
    ; "*", `Parse_error
    ; "+", `Parse_error
    ; "?", `Parse_error
    ; "a{}", `Parse_error
    ; "a{2", `Parse_error
    ; "a{3,2}", `Parse_error
    ; ")", `Parse_error
    ; "[", `Parse_error
    ; "[[.", `Parse_error
    ; "[[.a.", `Parse_error
    ; "[[.ab.]]", `Not_supported
    ; "[[:unknown:]]", `Parse_error
    ; "[[:digit", `Parse_error
    ; "[[:digit:x]", `Parse_error
    ];
  let re = Re.Posix.compile_pat "(a){1,3}" in
  assert (Re.Group.all_offset (Re.exec re "aaaa") = [| 0, 3; 2, 3 |]);
  let re = Re.Posix.compile_pat ~opts:[ `ICase; `NoSub ] "(ab)" in
  assert (Re.execp re "AB" && Re.group_count re = 1);
  List.iter
    (fun newline ->
       let opts = if newline then [ `Newline ] else [] in
       assert (Re.execp (Re.Posix.compile_pat ~opts ".") "\n" = not newline);
       assert (Re.execp (Re.Posix.compile_pat ~opts "^a$") "x\na\ny" = newline))
    [ false; true ];
  assert (not (matches (Re.Posix.re ~opts:[ `Newline ]) "[^a]" "\n"));
  [%expect {| |}]
;;

let%expect_test "Emacs case folding and trailing unmatched group" =
  assert (matches (Re.Emacs.re ~case:false) "aBc" "AbC");
  assert (not (matches (Re.Emacs.re ~case:true) "aBc" "AbC"));
  assert (Re.execp (Re.Emacs.compile_pat ~case:false "abc") "ABC");
  assert (not (Re.execp (Re.Emacs.compile_pat "abc") "ABC"));
  assert (Re.Emacs.re_result {|\)|} = Error `Parse_error);
  [%expect {| |}]
;;
