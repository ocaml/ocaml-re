open Import
module Pcre = Re_private.Pcre

let%expect_test "quoted strings" =
  let test re s =
    match Pcre.re re with
    | exception _ -> Format.printf "failed to parse@."
    | re -> t re s
  in
  test {|\Qfoo\E|} "foo";
  [%expect {| (Group (foo (0 3))) |}];
  test {|\Qbar|} "";
  [%expect {| failed to parse |}];
  test {|\Qbaz\|} "";
  [%expect {| failed to parse |}];
  test {|\Qba\Xz\E|} {|ba\Xz|};
  [%expect {| (Group (ba\Xz (0 5))) |}]
;;
