open Import

let%expect_test "a quantifier applies to the last quoted metacharacter" =
  (* The final + quantifies the literal +, so the match should be "a+++". *)
  let re = Re.Perl.compile_pat {|\Qa+\E+|} in
  printf "%S\n" (Re.Group.get (Re.exec re "a+++") 0);
  [%expect {| "a+++" |}]
;;
