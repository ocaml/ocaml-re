module Fmt = Re_private.Fmt

let%expect_test "format-to-string flushes buffered output" =
  List.iter
    (fun s -> assert (Fmt.to_to_string Fmt.str s = s))
    [ ""; "hello"; String.make 100 'a' ];
  assert (Fmt.to_to_string Fmt.int 42 = "42");
  assert (Fmt.to_to_string (Fmt.array ~pp_sep:(Fmt.lit ",") Fmt.int) [| 1; 2 |] = "1,2");
  [%expect {| |}]
;;
