open Import
module Pcre = Re_private.Pcre

let test re s =
  match Pcre.re re with
  | exception _ -> Format.printf "failed to parse@."
  | re -> t re s
;;

let%expect_test "quoted strings" =
  test {|\Qfoo\E|} "foo";
  [%expect {| (Group (foo (0 3))) |}];
  test {|\Qbar|} "";
  [%expect {| failed to parse |}];
  test {|\Qbaz\|} "";
  [%expect {| failed to parse |}];
  test {|\Qba\Xz\E|} {|ba\Xz|};
  [%expect {| (Group (ba\Xz (0 5))) |}]
;;

let%expect_test "octal" =
  test {|\025|} (String.make 1 '\o025');
  [%expect {| (Group ( (0 1))) |}];
  test {|\999|} "";
  [%expect {| failed to parse |}];
  test {|\111|} (String.make 1 '\o111');
  [%expect {| (Group (I (0 1))) |}]
;;
