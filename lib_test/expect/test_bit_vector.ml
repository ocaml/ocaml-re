open! Import
module Bit_vector = Re_private.Bit_vector

let%expect_test "reset_zero" =
  let n = Bit_vector.create_zero 10 in
  let print () = Format.printf "%a@." Bit_vector.pp n in
  print ();
  [%expect {|
    (len 10)
    (bits "\000\000") |}];
  Bit_vector.reset_zero n;
  print ();
  [%expect {|
    (len 10)
    (bits "\000\000") |}];
  Bit_vector.set n 1 true;
  print ();
  [%expect {|
    (len 10)
    (bits "\002\000") |}];
  Bit_vector.reset_zero n;
  print ();
  [%expect {|
    (len 10)
    (bits "\000\000") |}]
;;
