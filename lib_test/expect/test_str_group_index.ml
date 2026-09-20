module Re = Re_private.Re

let%expect_test "Str group offsets reject an existing group above nine" =
  let input = "abcdefghij" in
  let pattern =
    List.init (String.length input) (fun i -> Printf.sprintf "\\(%c\\)" input.[i])
    |> String.concat ""
  in
  assert (Str.string_match (Str.regexp pattern) input 0);
  assert (Re.Str.string_match (Re.Str.regexp pattern) input 0);
  List.iter
    (fun group ->
       List.iter
         (fun (name, beginning, ending) ->
            let report operation f =
              let result =
                try string_of_int (f group) with
                | exn -> Printexc.to_string exn
              in
              Printf.printf "%s.%s %d: %s\n" name operation group result
            in
            report "group_beginning" beginning;
            report "group_end" ending)
         [ "Str", Str.group_beginning, Str.group_end
         ; "Re.Str", Re.Str.group_beginning, Re.Str.group_end
         ])
    [ 9; 10 ];
  (* Group 10 exists and occupies [9,10), just as Str reports. The one-digit
     limit for replacement syntax does not restrict these offset accessors. *)
  [%expect
    {|
    Str.group_beginning 9: 8
    Str.group_end 9: 9
    Re.Str.group_beginning 9: 8
    Re.Str.group_end 9: 9
    Str.group_beginning 10: 9
    Str.group_end 10: 10
    Re.Str.group_beginning 10: Invalid_argument("Str.group_beginning")
    Re.Str.group_end 10: Invalid_argument("Str.group_end")
    |}]
;;
