open Import
open Re

let print_parse pattern =
  match Perl.re_result pattern with
  | Ok re -> Format.printf "%S: %a@." pattern pp re
  | Error `Parse_error -> Printf.printf "%S: Parse_error\n" pattern
  | Error `Not_supported -> Printf.printf "%S: Not_supported\n" pattern
;;

let%expect_test "atom dispatch preserves delimiters and quantifier lookahead" =
  List.iter
    [ ""
    ; "|"
    ; "(|a)"
    ; "(a|)b"
    ; "a*|b+"
    ; "a{2,3}?b"
    ; "a\\{b"
    ; "a\\|b"
    ; "\\Qab\\E*"
    ; "(?:a)(b)c"
    ; "a]}"
    ]
    ~f:print_parse;
  [%expect
    {|
    "": (Sequence )
    "|": (Alternative (Sequence )(Sequence ))
    "(|a)": (Group (Alternative (Sequence )(Set 97)))
    "(a|)b": (Sequence (Group (Alternative (Set 97)(Sequence )))(Set 98))
    "a*|b+": (Alternative (Sem_greedy Greedy (Repeat (Set 97) 0))
                (Sem_greedy Greedy (Repeat (Set 98) 1)))
    "a{2,3}?b": (Sequence (Sem_greedy Non_greedy (Repeat (Set 97) 2 3))(Set 98))
    "a\\{b": (Sequence (Set 97)(Set 123)(Set 98))
    "a\\|b": (Sequence (Set 97)(Set 124)(Set 98))
    "\\Qab\\E*": (Sem_greedy Greedy (Repeat (Sequence (Set 97)(Set 98)) 0))
    "(?:a)(b)c": (Sequence (Set 97)(Group (Set 98))(Set 99))
    "a]}": (Sequence (Set 97)(Set 93)(Set 125))
    |}]
;;

let%expect_test "atom dispatch records malformed and unsupported syntax" =
  List.iter
    [ "("
    ; ")"
    ; "a)"
    ; "*"
    ; "+"
    ; "?"
    ; "a**"
    ; "a{"
    ; "a{q"
    ; "a{2"
    ; "a{3,2}"
    ; "(?:a"
    ; "(?"
    ; "a\\"
    ; "[a"
    ; "\\x4"
    ; "\\1"
    ; "\\8"
    ; "[\\1]"
    ]
    ~f:print_parse;
  [%expect
    {|
    "(": Parse_error
    ")": Parse_error
    "a)": Parse_error
    "*": Parse_error
    "+": Parse_error
    "?": Parse_error
    "a**": Parse_error
    "a{": Parse_error
    "a{q": Parse_error
    "a{2": Parse_error
    "a{3,2}": Parse_error
    "(?:a": Parse_error
    "(?": Parse_error
    "a\\": Parse_error
    "[a": Parse_error
    "\\x4": (Set 4)
    "\\1": Not_supported
    "\\8": Not_supported
    "[\\1]": (Set 1)
    |}]
;;
