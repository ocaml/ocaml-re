open Import
module Cset = Re_private.Cset
module Color_map = Re_private.Color_map

let colors sets =
  let map = Color_map.make () in
  List.iter sets ~f:(Color_map.split map);
  Color_map.flatten map
;;

let%expect_test "repeated singleton and complement partitions cover all bytes" =
  let singles = List.init ~len:256 ~f:(fun i -> Cset.single (Cset.of_int i)) in
  let sets =
    List.concat_map singles ~f:(fun set ->
      [ set; set; Cset.diff Cset.cany set; set; Cset.diff Cset.cany set ])
  in
  let table, _, repr = colors sets in
  assert (Color_map.Repr.length repr = 256);
  for i = 0 to 255 do
    assert (Char.code (Color_map.Table.get_char table (Cset.of_int i)) = i)
  done;
  [%expect {||}]
;;

let%expect_test "deduplication preserves mixed-set equivalence classes" =
  let sets =
    Cset.[ empty; cany; cdigit; cseq 'a' 'z'; csingle 'a'; diff cany (csingle 'b') ]
  in
  let repeated = List.concat_map sets ~f:(fun set -> [ set; set; set ]) in
  let table, _, _ = colors repeated in
  for i = 0 to 255 do
    for j = 0 to 255 do
      let same_membership =
        List.for_all sets ~f:(fun set ->
          Bool.equal (Cset.mem (Cset.of_int i) set) (Cset.mem (Cset.of_int j) set))
      in
      let same_color =
        Char.equal
          (Color_map.Table.get_char table (Cset.of_int i))
          (Color_map.Table.get_char table (Cset.of_int j))
      in
      assert (Bool.equal same_membership same_color)
    done
  done;
  [%expect {||}]
;;

let%expect_test "singleton tracking is local to one compilation" =
  let single = Cset.csingle 'a' in
  let _, _, a = colors [ single; single ] in
  let _, _, b = colors [ single; single ] in
  assert (Color_map.Repr.length a = 2);
  assert (Color_map.Repr.length b = 2);
  [%expect {||}]
;;
