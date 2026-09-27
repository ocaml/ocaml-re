open Import
open Re

let outcome pattern =
  match Emacs.re_result pattern with
  | Ok re -> Format.asprintf "PARSED: %a" pp re
  | Error `Parse_error -> "Parse_error"
  | Error `Not_supported -> "Not_supported"
;;

let print_unsupported patterns =
  List.iter patterns ~f:(fun pattern ->
    Printf.printf "%S: %s\n" pattern (outcome pattern))
;;

let%expect_test "explicitly numbered groups are not supported" =
  (* Emacs: "\(?NUM:...\)" pins a group number. [Re.Emacs] numbers groups by
     position only. *)
  print_unsupported [ {|\(?1:a\)|}; {|\(?2:ab\)|} ];
  [%expect
    {|
    "\\(?1:a\\)": Not_supported
    "\\(?2:ab\\)": Not_supported
    |}]
;;

let%expect_test "backreferences are not supported" =
  (* Emacs: "\1" through "\9" match the corresponding captured group. *)
  print_unsupported [ {|\(a\)\1|}; {|\(a*\)\1|}; {|\(a\)\(b\)\2\1|} ];
  [%expect
    {|
    "\\(a\\)\\1": Not_supported
    "\\(a*\\)\\1": Not_supported
    "\\(a\\)\\(b\\)\\2\\1": Not_supported
    |}]
;;

let%expect_test "syntax and category classes are not supported" =
  (* Emacs: "\sCODE" and "\SCODE" match characters by syntax class, and
     "\cCODE"/"\CCODE" by category. Both depend on the buffer's syntax and
     category tables, which [Re.Emacs] has no access to. *)
  print_unsupported
    [ "\\s-"
    ; "\\s "
    ; {|\s.|}
    ; {|\sw|}
    ; {|\s_|}
    ; {|\s(|}
    ; {|\s)|}
    ; {|\s"|}
    ; {|\s<|}
    ; {|\s>|}
    ; {|\s@|}
    ; {|\s$|}
    ; {|\s!|}
    ; {|\s'|}
    ; {|\s/|}
    ; "\\s|"
    ; {|\S-|}
    ; {|\Sw|}
    ; {|\cC|}
    ; {|\cc|}
    ; {|\Cg|}
    ; {|\cA|}
    ];
  [%expect
    {|
    "\\s-": Not_supported
    "\\s ": Not_supported
    "\\s.": Not_supported
    "\\sw": Not_supported
    "\\s_": Not_supported
    "\\s(": Not_supported
    "\\s)": Not_supported
    "\\s\"": Not_supported
    "\\s<": Not_supported
    "\\s>": Not_supported
    "\\s@": Not_supported
    "\\s$": Not_supported
    "\\s!": Not_supported
    "\\s'": Not_supported
    "\\s/": Not_supported
    "\\s|": Not_supported
    "\\S-": Not_supported
    "\\Sw": Not_supported
    "\\cC": Not_supported
    "\\cc": Not_supported
    "\\Cg": Not_supported
    "\\cA": Not_supported
    |}]
;;

let%expect_test "symbol boundaries are not supported" =
  (* Emacs: "\_<" and "\_>" match symbol boundaries, which depend on the
     symbol syntax class of the buffer. *)
  print_unsupported [ {|\_<|}; {|\_>|}; {|\_<foo|}; {|foo\_>|} ];
  [%expect
    {|
    "\\_<": Not_supported
    "\\_>": Not_supported
    "\\_<foo": Not_supported
    "foo\\_>": Not_supported
    |}]
;;
