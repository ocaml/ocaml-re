open Import

let () = Printexc.record_backtrace true

module Hash_set = Re_private.Hash_set

let id1 = 1
let id2 = 2
let id3 = 3

let test table f =
  if f table
  then print_endline "[PASS]"
  else (
    print_endline "[FAIL]";
    Format.printf "%a@." Hash_set.pp table)
;;

let%expect_test "basic set" =
  let set = Hash_set.create () in
  test set Hash_set.is_empty;
  [%expect {| [PASS] |}];
  test set (fun set -> not (Hash_set.mem set id1));
  [%expect {|
    [PASS] |}]
;;

let%expect_test "add 1 element" =
  let set = Hash_set.create () in
  Hash_set.add set id1;
  test set (fun set -> not (Hash_set.is_empty set));
  [%expect {|
    [PASS] |}];
  test set (fun set -> Hash_set.mem set id1);
  [%expect {|
    [PASS] |}];
  Hash_set.add set id1;
  test set (fun set -> Hash_set.mem set id1);
  [%expect {| [PASS] |}];
  Hash_set.add set id2;
  test set (fun set -> Hash_set.mem set id2);
  [%expect {| [PASS] |}];
  Hash_set.add set id3;
  test set (fun set -> Hash_set.mem set id3);
  [%expect {|
    [PASS] |}];
  test set (fun set -> List.for_all [ id1; id2; id3 ] ~f:(fun id -> Hash_set.mem set id));
  [%expect {| [PASS] |}]
;;
