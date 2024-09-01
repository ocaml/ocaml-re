open! Import

let%expect_test "fixed repetition" =
  let re = Re.compile @@ Re.(repn (char 'a') 3 (Some 3)) in
  let test s = printf "%b\n" (Re.execp re s) in
  test "";
  [%expect {| false |}];
  test "aa";
  [%expect {| false |}];
  test "aaa";
  [%expect {| true |}];
  test "aaaa";
  [%expect {| true |}]
;;
