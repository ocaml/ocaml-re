let%expect_test "globstars must not bypass leading-period restrictions" =
  Test_glob.glob ~anchored:true "**/*" ".hidden";
  [%expect {| true |}]
;;

let%expect_test "leading-period restrictions also apply without pathname matching" =
  Test_glob.glob ~anchored:true ~pathname:false "*.txt" ".txt";
  [%expect {| true |}]
;;

let%expect_test "brace expansion preserves braces without alternatives" =
  Test_glob.glob ~anchored:true ~expand_braces:true "{foo}" "{foo}";
  [%expect {| false |}]
;;
