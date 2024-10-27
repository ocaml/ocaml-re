open Import
module Ast = Re_private.Ast

let test f string =
  match f string with
  | Ok res -> print_dyn (Ast.to_dyn res)
  | Error _ -> assert false
;;

let%expect_test "pcre" =
  test Re.Pcre.re_result "(a|b|c)";
  [%expect {| (Group (Set (Cast (Alternative (Cset 97) (Cset 98) (Cset 99))))) |}]
;;

let%expect_test "emacs" =
  test Re.Emacs.re_result {|\(a\|b\|c\)|};
  [%expect {| (Group (Set (Cast (Alternative (Cset 97) (Cset 98) (Cset 99))))) |}]
;;

let%expect_test "perl" =
  test Re.Perl.re_result "(a|b|c)";
  [%expect {| (Group (Set (Cast (Alternative (Cset 97) (Cset 98) (Cset 99))))) |}]
;;

let%expect_test "posix" =
  test Re.Posix.re_result "(a|b|c)";
  [%expect {| (Group (Set (Cast (Alternative (Cset 97) (Cset 98) (Cset 99))))) |}]
;;
