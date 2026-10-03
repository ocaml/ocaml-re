open Import

(*
 * Tests based on description of emacs regular expressions given at
 *   http://www.gnu.org/manual/elisp-manual-20-2.5/html_chapter/elisp_34.html
 *)

let re re = Format.printf "%a@." Re.pp (Re.Emacs.re re)

let%expect_test "not supported" =
  let re s =
    try ignore (Re.Emacs.re s) with
    | Re.Emacs.Parse_error -> print_endline "Parse error"
    | Re.Emacs.Not_supported -> print_endline "Not supported"
  in
  re "\\0";
  [%expect {| Not supported |}]
;;

let%expect_test "escaping special characters" =
  re "\\.";
  [%expect {| (Set 46) |}];
  re "\\*";
  [%expect {| (Set 42) |}];
  re "\\+";
  [%expect {| (Set 43) |}];
  re "\\?";
  [%expect {| (Set 63) |}];
  re "\\[";
  [%expect {| (Set 91) |}];
  re "\\]";
  [%expect {| (Set 93) |}];
  re "\\^";
  [%expect {| (Set 94) |}];
  re "\\$";
  [%expect {| (Set 36) |}];
  re "\\\\";
  [%expect {| (Set 92) |}]
;;

let%expect_test "special characeters" =
  re ".";
  [%expect {| (Set 0-9, 11-255) |}];
  re "a*";
  [%expect {| (Repeat (Set 97) 0) |}];
  re "a+";
  [%expect {| (Repeat (Set 97) 1) |}];
  re "a?";
  [%expect {| (Repeat (Set 97) 0 1) |}];
  re "[ab]";
  [%expect {| (Alternative (Set 98)(Set 97)) |}];
  re "[a-z]";
  [%expect {| (Set 97-122) |}];
  re "[a-z$%.]";
  [%expect {| (Alternative (Set 46)(Set 37)(Set 36)(Set 97-122)) |}];
  re "[]a]";
  [%expect {| (Alternative (Set 97)(Set 93)) |}];
  re "[]-]";
  [%expect {| (Alternative (Set 93)(Set 45)) |}];
  re "[a^]";
  [%expect {| (Alternative (Set 94)(Set 97)) |}];
  re "[^a-z]";
  [%expect {| (Complement (Set 97-122)) |}];
  re "[^a-z$]";
  [%expect {| (Complement (Set 36)(Set 97-122)) |}];
  re "^";
  [%expect {| Beg_of_line |}];
  re "$";
  [%expect {| End_of_line |}]
;;

let%expect_test "alternatives" =
  re "a\\|b";
  [%expect {| (Alternative (Set 97)(Set 98)) |}];
  re "aa\\|bb";
  [%expect {| (Alternative (Sequence (Set 97)(Set 97))(Sequence (Set 98)(Set 98))) |}]
;;

let%expect_test "contexts" =
  re "\\`";
  [%expect {| Beg_of_str |}];
  re "\\'";
  [%expect {| End_of_str |}];
  re "\\=";
  [%expect {| Start |}];
  re "\\b";
  [%expect {| (Alternative Beg_of_wordEnd_of_word) |}];
  re "\\B";
  [%expect {| Not_bound |}];
  re "\\<";
  [%expect {| Beg_of_word |}];
  re "\\>";
  [%expect {| End_of_word |}]
;;

let%expect_test "word-constituent" =
  re "\\w";
  [%expect
    {|
    (Alternative
       (Set 48-57, 65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255)
       (Set 95)) |}];
  re "\\W";
  [%expect
    {|
    (Complement
       (Set 48-57, 65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255)
       (Set 95)) |}]
;;

let%expect_test "grouping" =
  re "\\(a\\)";
  [%expect {| (Group (Set 97)) |}];
  re "\\(a\\|b\\)c";
  [%expect {| (Sequence (Group (Alternative (Set 97)(Set 98)))(Set 99)) |}]
;;

let%expect_test "concatenation" =
  re "ab";
  [%expect {| (Sequence (Set 97)(Set 98)) |}]
;;

let%expect_test "ordinary characters" =
  re "a";
  [%expect {| (Set 97) |}]
;;

let outcome pattern subject =
  match Re.Emacs.re_result pattern with
  | Error `Parse_error -> "parse error"
  | Error `Not_supported -> "not supported"
  | Ok re ->
    (match Re.exec_opt (Re.compile re) subject with
     | None -> "no match"
     | Some groups -> Printf.sprintf "match %S" (Re.Group.get groups 0))
;;

let show pattern subject =
  Printf.printf "%S on %S: %s\n" pattern subject (outcome pattern subject)
;;

let%expect_test "classes, intervals and escapes" =
  List.iter
    [ {|[[:alpha:]]|}, "a"
    ; {|[[:alpha:]]|}, "a]"
    ; {|[[:digit:]]|}, "7"
    ; {|^a\{2\}$|}, "aa"
    ; {|a*?|}, "aaa"
    ; {|\(?:ab\)|}, "ab"
    ; {|\d|}, "d"
    ; {|[z-a]|}, "z"
    ; {|*foo|}, "*foo"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "[[:alpha:]]" on "a": match "a"
    "[[:alpha:]]" on "a]": match "a"
    "[[:digit:]]" on "7": match "7"
    "^a\\{2\\}$" on "aa": match "aa"
    "a*?" on "aaa": match ""
    "\\(?:ab\\)" on "ab": match "ab"
    "\\d" on "d": match "d"
    "[z-a]" on "z": no match
    "*foo" on "*foo": match "*foo"
    |}]
;;

let%expect_test "character classes" =
  List.iter
    [ {|[[:alnum:]]|}, "a"
    ; {|[[:blank:]]|}, "\t"
    ; {|[[:cntrl:]]|}, "\001"
    ; {|[[:graph:]]|}, "!"
    ; {|[[:lower:]]|}, "a"
    ; {|[[:print:]]|}, " "
    ; {|[[:punct:]]|}, "!"
    ; {|[[:space:]]|}, " "
    ; {|[[:upper:]]|}, "A"
    ; {|[[:xdigit:]]|}, "f"
    ; {|[[:word:]]|}, "_"
    ; {|[[:ascii:]]|}, "a"
    ; {|[[:nonascii:]]|}, "\x80"
    ; {|[[:unibyte:]]|}, "a"
    ; {|[[:multibyte:]]|}, "a"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "[[:alnum:]]" on "a": match "a"
    "[[:blank:]]" on "\t": match "\t"
    "[[:cntrl:]]" on "\001": match "\001"
    "[[:graph:]]" on "!": match "!"
    "[[:lower:]]" on "a": match "a"
    "[[:print:]]" on " ": match " "
    "[[:punct:]]" on "!": match "!"
    "[[:space:]]" on " ": match " "
    "[[:upper:]]" on "A": match "A"
    "[[:xdigit:]]" on "f": match "f"
    "[[:word:]]" on "_": match "_"
    "[[:ascii:]]" on "a": match "a"
    "[[:nonascii:]]" on "\128": match "\128"
    "[[:unibyte:]]" on "a": match "a"
    "[[:multibyte:]]" on "a": no match
    |}]
;;

let%expect_test "ranges, intervals and lazy repetition" =
  List.iter
    [ {|[a-z]|}, "q"
    ; {|[z-a]|}, "z"
    ; {|[z-a]|}, "a"
    ; {|^a\{2\}$|}, "aaa"
    ; {|^a\{2,\}$|}, "aaaa"
    ; {|^a\{,2\}$|}, ""
    ; {|^a\{,2\}$|}, "aa"
    ; {|^a\{,2\}$|}, "aaa"
    ; {|^a\{,\}$|}, "aaa"
    ; {|^a\{2,3\}$|}, "aaa"
    ; {|^a\{2,3\}$|}, "aaaa"
    ; {|^a*a|}, "aaa"
    ; {|^a*?a|}, "aaa"
    ; {|^a+a|}, "aaa"
    ; {|^a+?a|}, "aaa"
    ; {|^a?a|}, "aa"
    ; {|^a??a|}, "aa"
    ; {|\-|}, "-"
    ; {|\q|}, "q"
    ; {|+foo|}, "+foo"
    ; {|?foo|}, "?foo"
    ; {|^*foo|}, "*foo"
    ; {|^*foo|}, "foo"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "[a-z]" on "q": match "q"
    "[z-a]" on "z": no match
    "[z-a]" on "a": no match
    "^a\\{2\\}$" on "aaa": no match
    "^a\\{2,\\}$" on "aaaa": match "aaaa"
    "^a\\{,2\\}$" on "": match ""
    "^a\\{,2\\}$" on "aa": match "aa"
    "^a\\{,2\\}$" on "aaa": no match
    "^a\\{,\\}$" on "aaa": match "aaa"
    "^a\\{2,3\\}$" on "aaa": match "aaa"
    "^a\\{2,3\\}$" on "aaaa": no match
    "^a*a" on "aaa": match "aaa"
    "^a*?a" on "aaa": match "a"
    "^a+a" on "aaa": match "aaa"
    "^a+?a" on "aaa": match "aa"
    "^a?a" on "aa": match "aa"
    "^a??a" on "aa": match "a"
    "\\-" on "-": match "-"
    "\\q" on "q": match "q"
    "+foo" on "+foo": match "+foo"
    "?foo" on "?foo": match "?foo"
    "^*foo" on "*foo": match "*foo"
    "^*foo" on "foo": no match
    |}]
;;

let%expect_test "shy groups do not capture" =
  let groups = Re.exec (Re.Emacs.compile_pat {|\(?:ab\)\(c\)|}) "abc" in
  Array.iter (Printf.printf "%S\n") (Re.Group.all groups);
  [%expect
    {|
    "abc"
    "c"
    |}]
;;

let%expect_test "optional interval repetition" =
  List.iter
    [ {|^a\{2,3\}?$|}, ""
    ; {|^a\{2,3\}?$|}, "aa"
    ; {|^a\{2,3\}?$|}, "aaa"
    ; {|^a\{2,3\}?$|}, "aaaa"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "^a\\{2,3\\}?$" on "": match ""
    "^a\\{2,3\\}?$" on "aa": match "aa"
    "^a\\{2,3\\}?$" on "aaa": match "aaa"
    "^a\\{2,3\\}?$" on "aaaa": no match
    |}]
;;
