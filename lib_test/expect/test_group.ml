open Import

let t re s =
  let group = Re.exec_opt (Re.compile re) s in
  Format.printf "%a@." (Fmt.opt Re.Group.pp) group
;;

open Re

let%expect_test "empty group" =
  let empty = group empty in
  t empty "";
  [%expect {| <None> |}];
  t empty "x";
  [%expect {| <None> |}]
;;

let%expect_test "zero length group" =
  let empty = group bos in
  t empty "";
  [%expect {| (Group ( (0 0))( (0 0))) |}];
  t empty "x";
  [%expect {| (Group ( (0 0))( (0 0))) |}]
;;

let%expect_test "no group" =
  let re = any in
  t re "";
  [%expect {| <None> |}];
  t re ".";
  [%expect {| (Group (. (0 1))) |}]
;;

let%expect_test "two groups" =
  let re = seq [ group any; group any ] in
  t re "a";
  [%expect {| <None> |}];
  t re "ab";
  [%expect {| (Group (ab (0 2))(a (0 1))(b (1 2))) |}];
  t re "abc";
  [%expect {| (Group (ab (0 2))(a (0 1))(b (1 2))) |}]
;;

let%expect_test "maybe group" =
  let twoany = seq [ any; any ] in
  let re = alt [ twoany; group twoany ] in
  t re "aa";
  [%expect {| (Group (aa (0 2))( (-1 -1))) |}];
  t re "a";
  [%expect {| <None> |}]
;;

let%expect_test "nesting of groups" =
  let re = group (seq [ group (char 'a'); char 'b' ]) in
  t re "ab";
  [%expect {| (Group (ab (0 2))(ab (0 2))(a (0 1))) |}]
;;
