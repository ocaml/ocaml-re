[@@@alert "-deprecated"]

module Re = Re_private.Re

let collect gen =
  let rec loop remaining acc =
    if remaining = 0 then failwith "generator did not terminate";
    match gen () with
    | None ->
      assert (gen () = None && gen () = None);
      List.rev acc
    | Some x -> loop (remaining - 1) (x :: acc)
  in
  loop 100 []
;;

let offsets = List.map Re.Group.all_offset

let tokens xs =
  List.map
    (function
      | `Text s -> `Text s
      | `Delim g -> `Delim (Re.Group.all_offset g))
    xs
;;

let%expect_test "generators, sequences and lists agree for every input window" =
  List.iter
    (fun pattern ->
       let re = Re.compile pattern in
       List.iter
         (fun s ->
            for pos = 0 to String.length s do
              for len = 0 to String.length s - pos do
                let all = offsets (Re.all ~pos ~len re s) in
                assert (offsets (collect (Re.all_gen ~pos ~len re s)) = all);
                assert (offsets (List.of_seq (Re.Seq.all ~pos ~len re s)) = all);
                let matches = Re.matches ~pos ~len re s in
                assert (collect (Re.matches_gen ~pos ~len re s) = matches);
                assert (List.of_seq (Re.Seq.matches ~pos ~len re s) = matches);
                let split = Re.split ~pos ~len re s in
                assert (collect (Re.split_gen ~pos ~len re s) = split);
                assert (List.of_seq (Re.Seq.split ~pos ~len re s) = split);
                let full = tokens (Re.split_full ~pos ~len re s) in
                assert (tokens (collect (Re.split_full_gen ~pos ~len re s)) = full);
                assert (tokens (List.of_seq (Re.Seq.split_full ~pos ~len re s)) = full)
              done
            done)
         [ ""; "a"; "aaa"; ",a,,b,"; "ab" ])
    Re.[ empty; epsilon; char ','; group (char 'a'); rep (char 'a'); eos ];
  let re = Re.(compile (char 'a')) in
  let a = Re.matches_gen re "aa" in
  let b = Re.matches_gen re "a" in
  assert (a () = Some "a");
  assert (b () = Some "a");
  assert (b () = None);
  assert (a () = Some "a");
  assert (a () = None);
  assert (Re.matches re "aba" = [ "a"; "a" ]);
  List.iter
    (fun gen ->
       match collect gen with
       | exception Invalid_argument _ -> ()
       | _ -> failwith "invalid generator window accepted")
    [ (fun () -> (Re.matches_gen ~pos:(-1) re "a") ())
    ; (fun () -> (Re.split_gen ~len:(-1) re "a") ())
    ];
  [%expect {| |}]
;;
