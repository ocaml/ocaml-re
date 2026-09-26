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

let%expect_test "bounded repetition and lazy quantifiers are not supported" =
  (* Emacs: "\{m\}", "\{m,\}", "\{,n\}", "\{m,n\}" bound repetition, and a
     trailing "?" makes any quantifier lazy ("*?", "+?", "??", "\{m,n\}?").
     [Re.Emacs] implements only the greedy "*", "+" and "?" forms. *)
  print_unsupported
    [ {|a\{2\}|}
    ; {|a\{2,\}|}
    ; {|a\{,2\}|}
    ; {|a\{2,3\}|}
    ; {|a*?|}
    ; {|a+?|}
    ; {|a??|}
    ; {|a\{2,3\}?|}
    ];
  [%expect
    {|
    "a\\{2\\}": Parse_error
    "a\\{2,\\}": Parse_error
    "a\\{,2\\}": Parse_error
    "a\\{2,3\\}": Parse_error
    "a*?": Parse_error
    "a+?": Parse_error
    "a??": Parse_error
    "a\\{2,3\\}?": Parse_error
    |}]
;;

let%expect_test "shy and explicitly numbered groups are not supported" =
  (* Emacs: "\(?:...\)" is a shy group and "\(?NUM:...\)" pins a group
     number. [Re.Emacs] accepts only the plain capturing "\(...\)". *)
  print_unsupported [ {|\(?:a\)|}; {|\(?:a\|b\)c|}; {|\(?1:a\)|}; {|\(?2:ab\)|} ];
  [%expect
    {|
    "\\(?:a\\)": Parse_error
    "\\(?:a\\|b\\)c": Parse_error
    "\\(?1:a\\)": Parse_error
    "\\(?2:ab\\)": Parse_error
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
     "\cCODE"/"\CCODE" by category. [Re.Emacs] works on fixed byte sets and
     has neither. *)
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
    "\\s-": Parse_error
    "\\s ": Parse_error
    "\\s.": Parse_error
    "\\sw": Parse_error
    "\\s_": Parse_error
    "\\s(": Parse_error
    "\\s)": Parse_error
    "\\s\"": Parse_error
    "\\s<": Parse_error
    "\\s>": Parse_error
    "\\s@": Parse_error
    "\\s$": Parse_error
    "\\s!": Parse_error
    "\\s'": Parse_error
    "\\s/": Parse_error
    "\\s|": Parse_error
    "\\S-": Parse_error
    "\\Sw": Parse_error
    "\\cC": Parse_error
    "\\cc": Parse_error
    "\\Cg": Parse_error
    "\\cA": Parse_error
    |}]
;;

let%expect_test "symbol boundaries are not supported" =
  (* Emacs: "\_<" and "\_>" match the beginning and end of a symbol. *)
  print_unsupported [ {|\_<|}; {|\_>|}; {|\_<foo|}; {|foo\_>|} ];
  [%expect
    {|
    "\\_<": Parse_error
    "\\_>": Parse_error
    "\\_<foo": Parse_error
    "foo\\_>": Parse_error
    |}]
;;

let%expect_test "POSIX named character classes are silently misparsed" =
  (* Emacs: "[[:NAME:]]" selects a named class inside a bracket expression.
     [Re.Emacs] has no named classes, so it parses "[[:alpha:]]" as a
     bracket expression of the literal characters "[", ":" and the letters
     of the name, followed by a literal "]". The pattern is accepted but
     means something else. *)
  let show pattern subject =
    Printf.printf
      "%S on %S: %b\n"
      pattern
      subject
      (Re.execp (Emacs.compile_pat pattern) subject)
  in
  show {|[[:alpha:]]|} "a";
  show {|[[:alpha:]]|} "a]";
  show {|[[:alpha:]]|} "[]";
  show {|[[:digit:]]|} "7";
  show {|[[:digit:]]|} "d]";
  show {|[[:space:]]|} " ";
  show {|[[:space:]]|} ":]";
  show {|[^[:alpha:]]|} "a]";
  show {|[^[:alpha:]]|} "z]";
  [%expect
    {|
    "[[:alpha:]]" on "a": false
    "[[:alpha:]]" on "a]": true
    "[[:alpha:]]" on "[]": true
    "[[:digit:]]" on "7": false
    "[[:digit:]]" on "d]": true
    "[[:space:]]" on " ": false
    "[[:space:]]" on ":]": true
    "[^[:alpha:]]" on "a]": false
    "[^[:alpha:]]" on "z]": true
    |}]
;;

let%expect_test "backslash-quoted ordinary characters are not supported" =
  (* Emacs: a backslash before an ordinary character matches that
     character, so "\d", "\-", "\%" and "\q" are literals. [Re.Emacs]
     accepts a backslash only before the punctuation metacharacters and
     raises on the rest. *)
  print_unsupported [ {|\d|}; {|\-|}; {|\%|}; {|\q|} ];
  [%expect
    {|
    "\\d": Parse_error
    "\\-": Parse_error
    "\\%": Parse_error
    "\\q": Parse_error
    |}]
;;

let%expect_test "repetition operators in literal position are not supported" =
  (* Emacs: a leading repetition operator is treated as an ordinary character
     for historical compatibility, so "*foo", "+foo" and "?foo" match those
     literals. [Re.Emacs] raises a parse error. *)
  print_unsupported [ {|*foo|}; {|+foo|}; {|?foo|} ];
  [%expect
    {|
    "*foo": Parse_error
    "+foo": Parse_error
    "?foo": Parse_error
    |}]
;;

let%expect_test "anchors are special in more positions than Emacs allows" =
  (* Emacs: "^" is special only at the start or after "\(...\)", "\(?:" or
     "\|", and "$" only at the end or before "\)" or "\|". Elsewhere they are
     ordinary characters. [Re.Emacs] always treats them as anchors, so these
     patterns parse but never match the literal text Emacs matches. *)
  let matches pattern subject = Re.execp (Emacs.compile_pat pattern) subject in
  assert (not (matches {|a$b|} "a$b"));
  assert (not (matches {|a^b|} "a^b"));
  [%expect {||}]
;;

let%expect_test "descending character ranges are not empty" =
  (* Emacs: a range whose lower bound is greater than its upper bound is
     empty, so "[z-a]" matches nothing. [Re.Emacs] takes the larger endpoint
     as both bounds, so "[z-a]" matches "z". *)
  let matches pattern subject = Re.execp (Emacs.compile_pat pattern) subject in
  assert (matches {|[z-a]|} "z");
  [%expect {||}]
;;
