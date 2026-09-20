module Cset = Re_private.Cset

let%expect_test "subtracting one interval from disjoint intervals" =
  let left = Cset.set "ac" in
  let right = Cset.cseq 'a' 'c' in
  assert (Cset.mem (Cset.of_char 'a') right);
  assert (Cset.mem (Cset.of_char 'c') right);
  let difference = Cset.diff left right in
  Format.printf "%a@." Cset.pp difference;
  (* Both 'a' and 'c' are in the subtracted interval, so the result should
     be empty. This records the current incorrect retention of 'c' (99). *)
  [%expect {| 99 |}]
;;
