open Import
module Slice = Re_private.Slice

let%expect_test "drop_rev partially consumes a slice" =
  let slice = { Slice.s = "abcdef"; pos = 0; len = 6 } in
  List.iter [ 0; 1; 6 ] ~f:(fun count ->
    let remaining = Slice.L.drop_rev [ slice ] count in
    let text =
      List.rev_map remaining ~f:(fun { Slice.s; pos; len } -> String.sub s pos len)
      |> String.concat ""
    in
    Printf.printf "drop %d: %S\n" count text);
  [%expect
    {|
    drop 0: "abcdef"
    drop 1: "bcdef"
    drop 6: ""
    |}]
;;
