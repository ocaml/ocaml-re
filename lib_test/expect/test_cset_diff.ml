module Cset = Re_private.Cset

let%expect_test "subtracting one interval from disjoint intervals" =
  let left = Cset.set "ac" in
  let right = Cset.cseq 'a' 'c' in
  assert (Cset.mem (Cset.of_char 'a') right);
  assert (Cset.mem (Cset.of_char 'c') right);
  let difference = Cset.diff left right in
  Format.printf "%a@." Cset.pp difference;
  (* Both 'a' and 'c' are in the subtracted interval, so the result is empty. *)
  [%expect {| |}]
;;

let%expect_test "difference agrees with every pair of subsets of eight bytes" =
  List.iter
    (fun base ->
       let sets =
         Array.init 256 (fun mask ->
           let set = ref Cset.empty in
           for i = 0 to 7 do
             if mask land (1 lsl i) <> 0
             then set := Cset.add (Cset.of_int (base + i)) !set
           done;
           !set)
       in
       for a = 0 to 255 do
         for b = 0 to 255 do
           let actual = Cset.diff sets.(a) sets.(b) in
           let expected = sets.(a land lnot b) in
           if not (Cset.equal actual expected)
           then failwith (Printf.sprintf "difference: base=%d a=%d b=%d" base a b)
         done
       done)
    [ 0; 248 ];
  [%expect {| |}]
;;
