module Buffer = Re_private.Parse_buffer
module Marks = Re_private.Mark_infos
module Group = Re_private.Group

let%expect_test "integer parsing preserves delimiters and rejects overflow" =
  List.iter
    (fun (input, expected, rest) ->
       let b = Buffer.create input in
       assert (Buffer.integer b = expected);
       match rest with
       | None -> assert (Buffer.eos b)
       | Some c -> assert (Buffer.get b = c))
    [ "", None, None
    ; "x", None, Some 'x'
    ; "0", Some 0, None
    ; "123", Some 123, None
    ; "123,", Some 123, Some ','
    ; string_of_int max_int, Some max_int, None
    ];
  List.iter
    (fun input ->
       match Buffer.integer (Buffer.create input) with
       | exception Buffer.Parse_error -> ()
       | _ -> failwith ("integer overflow accepted: " ^ input))
    [ string_of_int max_int ^ "0"; String.make 100 '9' ];
  [%expect {| |}]
;;

let%expect_test "internal mark offsets distinguish absent and present indices" =
  let marks = Marks.make [ 0, 0; 1, 1; 4, 2; 5, 3 ] in
  let groups =
    Group.create "abc" ~gcount:3 ~gpos:[| 0; 1; 2; 3 |] marks Re_private.Pmark.Set.empty
  in
  List.iter
    (fun (i, expected) ->
       let start = Marks.start_offset marks i in
       let stop = Marks.stop_offset marks i in
       assert (Marks.Offset.is_present start = Option.is_some expected);
       assert (Marks.Offset.is_present stop = Option.is_some expected);
       let start = Group.start_offset groups i in
       let stop = Group.stop_offset groups i in
       assert (Group.Offset.is_present start = Option.is_some expected);
       assert (Group.Offset.is_present stop = Option.is_some expected);
       match expected with
       | None -> ()
       | Some (a, b) ->
         assert (Group.Offset.get_no_check start = a);
         assert (Group.Offset.get_no_check stop = b))
    [ 0, Some (0, 1); 1, None; 2, Some (2, 3); 3, None ];
  let category = Re_private.Category.from_char 'a' in
  assert (Re_private.Category.compare category category = 0);
  let module H = Re_private.Hash_set in
  let set = H.create () in
  H.add set 1;
  assert (Format.asprintf "%a" H.pp set = "(table 1)\n(size 1)\n");
  [%expect {| |}]
;;
