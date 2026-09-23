module Slice = Re_private.Slice

let contents slices =
  String.concat "" (List.map (fun (s : Slice.t) -> String.sub s.s s.pos s.len) slices)
;;

let%expect_test "slice substrings and prefix dropping agree with string operations" =
  let input = "abcdef" in
  let length = String.length input in
  for mask = 0 to (1 lsl (length - 1)) - 1 do
    let start = ref 0 in
    let slices = ref [] in
    for stop = 1 to length do
      if stop = length || mask land (1 lsl (stop - 1)) <> 0
      then (
        (* Include source offsets and empty slices, not just standalone chunks. *)
        let s = "xx" ^ String.sub input !start (stop - !start) ^ "yy" in
        slices
        := Slice.{ s = ""; pos = 0; len = 0 }
           :: Slice.{ s; pos = 2; len = stop - !start }
           :: !slices;
        start := stop)
    done;
    let slices = List.rev !slices in
    assert (contents slices = input);
    for start = 0 to length do
      for stop = start to length do
        assert (
          Slice.L.get_substring slices ~start ~stop = String.sub input start (stop - start))
      done;
      let dropped = Slice.L.drop_rev (List.rev slices) start |> List.rev in
      let expected = String.sub input start (length - start) in
      let actual = contents dropped in
      if actual <> expected
      then
        failwith
          (Printf.sprintf
             "mask=%d drop=%d: expected %S got %S"
             mask
             start
             expected
             actual)
    done
  done;
  assert (Slice.L.get_substring [] ~start:0 ~stop:0 = "");
  assert (Slice.L.drop_rev [] 1 = []);
  [%expect {| |}]
;;
