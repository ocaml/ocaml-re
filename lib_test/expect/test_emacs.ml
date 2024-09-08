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
  re "*ab";
  [%expect {| Parse error |}];
  re "+ab";
  [%expect {| Parse error |}];
  re "?ab";
  [%expect {| Parse error |}];
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
