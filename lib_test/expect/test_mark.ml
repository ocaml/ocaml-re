open Import
open Re

let test_mark ?pos ?len r s il1 il2 =
  let subs = exec ?pos ?len (compile r) s in
  Format.printf
    "%b@."
    (List.for_all ~f:(Mark.test subs) il1
     && List.for_all ~f:(fun x -> not (Mark.test subs x)) il2)
;;

let%expect_test "mark" =
  let i, r = mark digit in
  test_mark r "0" [ i ] [];
  [%expect {| true |}]
;;

let%expect_test "mark seq" =
  let i, r = mark digit in
  let r = seq [ r; r ] in
  test_mark r "02" [ i ] [];
  [%expect {| true |}]
;;

let%expect_test "mark rep" =
  let i, r = mark digit in
  let r = rep r in
  test_mark r "02" [ i ] [];
  [%expect {| true |}]
;;

let%expect_test "mark alt" =
  let ia, ra = mark (char 'a') in
  let ib, rb = mark (char 'b') in
  let r = alt [ ra; rb ] in
  test_mark r "a" [ ia ] [ ib ];
  test_mark r "b" [ ib ] [ ia ];
  [%expect {|
    true
    true |}];
  let r = rep r in
  test_mark r "ab" [ ia; ib ] [];
  [%expect {| true |}]
;;

let%expect_test "mark prefers lhs" =
  let two_chars = seq [ any; any ] in
  let lhs, x = mark two_chars in
  let rhs, x' = mark two_chars in
  let r = alt [ x; x' ] in
  test_mark r "aa" [ lhs ] [ rhs ];
  [%expect {| true |}]
;;
