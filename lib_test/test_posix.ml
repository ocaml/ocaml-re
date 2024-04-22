open OUnit

let execp = Re.execp

let test_class_space () =
  let re = Re.Posix.compile_pat {|a[[:space:]]b|} in
  let exec = Re.execp re in
  assert_bool "matches with space" (exec "a b");
  assert_bool "does not match without a space" (not (exec "ab"));
  assert_bool "does not match with a different char" (not (exec "a_b"))
;;

let suite = "posix" >::: [ "regression 213" >:: test_class_space ]
let () = ignore (run_test_tt_main suite)
