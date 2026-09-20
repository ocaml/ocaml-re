open Import
open Re
module Mark_map = Map.Make (Mark)

let names = ref Mark_map.empty

let mark name r =
  let mark, r = mark r in
  names := Mark_map.add mark name !names;
  r
;;

let test_mark ?pos ?len r s =
  exec ?pos ?len (compile r) s
  |> Mark.all
  |> Mark.Set.elements
  |> List.map ~f:(fun mark ->
    match Mark_map.find_opt mark !names with
    | None -> "?"
    | Some name -> name)
  |> List.sort ~cmp:String.compare
  |> String.concat " "
  |> print_endline
;;

let%expect_test "mark" =
  test_mark (mark "i" digit) "0";
  [%expect {| i |}]
;;

let%expect_test "mark seq" =
  let r = mark "i" digit in
  test_mark (seq [ r; r ]) "02";
  [%expect {| i |}]
;;

let%expect_test "mark rep" =
  test_mark (rep (mark "i" digit)) "02";
  [%expect {| i |}]
;;

let%expect_test "mark alt" =
  let r = alt [ mark "ia" (char 'a'); mark "ib" (char 'b') ] in
  test_mark r "a";
  [%expect {| ia |}];
  test_mark r "b";
  [%expect {| ib |}];
  test_mark (rep r) "ab";
  [%expect {| ia ib |}]
;;

let%expect_test "mark prefers lhs" =
  let two_chars = seq [ any; any ] in
  test_mark (alt [ mark "lhs" two_chars; mark "rhs" two_chars ]) "aa";
  [%expect {| lhs |}]
;;
