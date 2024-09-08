open Import

(* Tests based on description of Perl regular expressions given at
   http://www.perl.com/CPAN-local/doc/manual/html/pod/perlre.html *)

let re ?opts s = Format.printf "%a@." Re.pp (Re.Perl.re ?opts s)

let try_parse ?opts s =
  try
    ignore (Re.Perl.re ?opts s);
    print_endline "Prased successfully"
  with
  | Re.Perl.Parse_error -> print_endline "Parse error"
  | Re.Perl.Not_supported -> print_endline "Not supported"
;;

let%expect_test "escaping meta characters" =
  re "\\^";
  [%expect {| (Set 94) |}];
  re "\\.";
  [%expect {| (Set 46) |}];
  re "\\$";
  [%expect {| (Set 36) |}];
  re "\\|";
  [%expect {| (Set 124) |}];
  re "\\(";
  [%expect {| (Set 40) |}];
  re "\\)";
  [%expect {| (Set 41) |}];
  re "\\[";
  [%expect {| (Set 91) |}];
  re "\\]";
  [%expect {| (Set 93) |}];
  re "\\*";
  [%expect {| (Set 42) |}];
  re "\\+";
  [%expect {| (Set 43) |}];
  re "\\?";
  [%expect {| (Set 63) |}];
  re "\\\\";
  [%expect {| (Set 92) |}]
;;

let%expect_test "basic metacharacters" =
  re "^";
  [%expect {| Beg_of_str |}];
  re ".";
  [%expect {| (Set 0-9, 11-255) |}];
  re "$";
  [%expect {| End_of_str |}];
  re "a|b";
  [%expect {| (Alternative (Set 97)(Set 98)) |}];
  re "aa|bb";
  [%expect {| (Alternative (Sequence (Set 97)(Set 97))(Sequence (Set 98)(Set 98))) |}];
  re "(a)";
  [%expect {| (Group (Set 97)) |}];
  re "(a|b)c";
  [%expect {| (Sequence (Group (Alternative (Set 97)(Set 98)))(Set 99)) |}];
  re "[ab]";
  [%expect {| (Alternative (Set 98)(Set 97)) |}];
  re "[a-z]";
  [%expect {| (Set 97-122) |}];
  re "[a-z$%.]";
  [%expect {| (Alternative (Set 46)(Set 37)(Set 36)(Set 97-122)) |}];
  re "[-az]";
  [%expect {| (Alternative (Set 122)(Set 97)(Set 45)) |}];
  re "[az-]";
  [%expect {| (Alternative (Set 122)(Set 45)(Set 97)) |}];
  re "[a\\-z]";
  [%expect {| (Alternative (Set 122)(Set 45)(Set 97)) |}];
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
  re "[a-\\sz]";
  [%expect {| (Alternative (Set 122)(Set 97)(Set 45)(Set 9-13, 32)) |}]
;;

let%expect_test "greedy quantifiers" =
  re "a*";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0)) |}];
  re "a+";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 1)) |}];
  re "a?";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0 1)) |}];
  re "a{10}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10 10)) |}];
  re "a{10,}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10)) |}];
  re "a{10,12}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10 12)) |}]
;;

let%expect_test "non-greedy quantifiers" =
  re "a*?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0)) |}];
  re "a+?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 1)) |}];
  re "a??";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0 1)) |}];
  re "a{10}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10 10)) |}];
  re "a{10,}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10)) |}];
  re "a{10,12}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10 12)) |}]
;;

let%expect_test "character sets" =
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
       (Set 95)) |}];
  re "\\s";
  [%expect {| (Set 9-13, 32) |}];
  re "\\S";
  [%expect {| (Complement (Set 9-13, 32)) |}];
  re "\\d";
  [%expect {| (Set 48-57) |}];
  re "\\D";
  [%expect {| (Complement (Set 48-57)) |}]
;;

let%expect_test "zero-width assertions" =
  re "\\b";
  [%expect {| (Alternative Beg_of_wordEnd_of_word) |}];
  re "\\B";
  [%expect {| Not_bound |}];
  re "\\A";
  [%expect {| Beg_of_str |}];
  re "\\Z";
  [%expect {| Last_end_of_line |}];
  re "\\z";
  [%expect {| End_of_str |}];
  re "\\G";
  [%expect {| Start |}]
;;

let%expect_test "options" =
  re ~opts:[ `Anchored ] "a";
  [%expect {| (Sequence Start(Set 97)) |}];
  re ~opts:[ `Caseless ] "b";
  [%expect {| (No_case (Set 98)) |}];
  re ~opts:[ `Dollar_endonly ] "$";
  [%expect {| Last_end_of_line |}];
  re ~opts:[ `Dollar_endonly; `Multiline ] "$";
  [%expect {| End_of_line |}];
  re ~opts:[ `Dotall ] ".";
  [%expect {| (Set 0-255) |}];
  re ~opts:[ `Multiline ] "^";
  [%expect {| Beg_of_line |}];
  re ~opts:[ `Multiline ] "$";
  [%expect {| End_of_line |}];
  re ~opts:[ `Ungreedy ] "a*";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0)) |}];
  re ~opts:[ `Ungreedy ] "a*?";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0)) |}]
;;

let%expect_test "clustering" =
  re "(?:a)";
  [%expect {| (Set 97) |}];
  re "(?:a|b)c";
  [%expect {| (Sequence (Alternative (Set 97)(Set 98))(Set 99)) |}]
;;

let%expect_test "comment" =
  re "a(?#comment)b";
  [%expect {| (Sequence (Set 97)(Sequence )(Set 98)) |}];
  try_parse "(?#";
  [%expect {| Parse error |}]
;;

let%expect_test "backrefs" =
  try_parse "\\0";
  [%expect {| Not supported |}]
;;

let%expect_test "ordinary characters" =
  re "a";
  [%expect {| (Set 97) |}]
;;

let%expect_test "concacentation" =
  re "ab";
  [%expect {| (Sequence (Set 97)(Set 98)) |}]
;;

let%expect_test "sets in classes" =
  re "[a\\s]";
  [%expect {| (Alternative (Set 9-13, 32)(Set 97)) |}]
;;

let%expect_test "fixed bug" =
  (try ignore (Re.compile (Re.Perl.re "(.*?)(\\WPl|\\Bpl)(.*)")) with
   | _ -> failwith "bug in Re.handle_case");
  [%expect {||}]
;;
