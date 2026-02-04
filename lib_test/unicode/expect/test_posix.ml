open Import

let%expect_test "class space" =
  let re = Re.Posix.compile_pat {|a[[:space:]]b|} in
  let exec = Re.execp re in
  assert (exec "a b");
  assert (not (exec "ab"));
  assert (not (exec "a_b"));
  [%expect {||}]
;;
