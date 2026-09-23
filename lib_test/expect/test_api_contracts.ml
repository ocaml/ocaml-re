module Re = Re_private.Re

let invalid f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "expected Invalid_argument"
;;

let not_found f =
  match f () with
  | exception Not_found -> ()
  | _ -> failwith "expected Not_found"
;;

let%expect_test "execution bounds apply to every entry point" =
  let re = Re.(compile (str "ab")) in
  let calls =
    [ (fun pos len -> ignore (Re.exec ~pos ~len re "xabz"))
    ; (fun pos len -> ignore (Re.exec_opt ~pos ~len re "xabz"))
    ; (fun pos len -> ignore (Re.execp ~pos ~len re "xabz"))
    ; (fun pos len -> ignore (Re.exec_partial ~pos ~len re "xabz"))
    ; (fun pos len -> ignore (Re.exec_partial_detailed ~pos ~len re "xabz"))
    ]
  in
  List.iter
    (fun f ->
       List.iter
         (fun (pos, len) -> invalid (fun () -> f pos len))
         [ -1, -1; 0, -2; 3, 2; 5, 0 ])
    calls;
  for pos = 0 to 4 do
    for len = 0 to 4 - pos do
      let expected = pos <= 1 && pos + len >= 3 in
      assert (Re.execp ~pos ~len re "xabz" = expected);
      match Re.exec_opt ~pos ~len re "xabz" with
      | None ->
        assert (not expected);
        not_found (fun () -> Re.exec ~pos ~len re "xabz")
      | Some g -> assert (expected && Re.Group.offset g 0 = (1, 3))
    done
  done;
  assert (Re.execp ~pos:1 ~len:(-1) re "xabz");
  [%expect {| |}]
;;

let%expect_test "iteration and replacement bounds and selected windows" =
  let re = Re.(compile (char 'a')) in
  let calls =
    [ (fun pos len -> ignore (Re.all ~pos ~len re "aba"))
    ; (fun pos len -> ignore (Re.matches ~pos ~len re "aba"))
    ; (fun pos len -> ignore (List.of_seq (Re.Seq.all ~pos ~len re "aba")))
    ; (fun pos len -> ignore (Re.split ~pos ~len re "aba"))
    ; (fun pos len -> ignore (Re.split_delim ~pos ~len re "aba"))
    ; (fun pos len -> ignore (List.of_seq (Re.Seq.split_full ~pos ~len re "aba")))
    ; (fun pos len -> ignore (Re.replace_string ~pos ~len re ~by:"x" "aba"))
    ; (fun pos len -> ignore (Re.replace ~pos ~len re ~f:(fun _ -> "x") "aba"))
    ]
  in
  List.iter
    (fun f ->
       List.iter (fun (pos, len) -> invalid (fun () -> f pos len)) [ -1, 1; 0, -1; 2, 2 ])
    calls;
  let calls = ref [] in
  let f g =
    calls := Re.Group.offset g 0 :: !calls;
    "X"
  in
  assert (Re.replace ~pos:1 ~len:3 re ~f "zabaq" = "XbX");
  assert (List.rev !calls = [ 1, 2; 3, 4 ]);
  calls := [];
  assert (Re.replace ~pos:1 ~len:3 ~all:false re ~f "zabaq" = "Xba");
  assert (!calls = [ 1, 2 ]);
  calls := [];
  assert (Re.replace ~pos:1 ~len:0 re ~f "zabaq" = "");
  assert (!calls = []);
  assert (Re.replace_string ~pos:1 ~len:3 re ~by:"X" "zabaq" = "XbX");
  [%expect {| |}]
;;

let%expect_test "copying cold and warm regexps preserves groups and marks" =
  let mark_a, a = Re.(mark (group ~name:"a" (char 'a'))) in
  let mark_b, b = Re.(mark (group ~name:"b" (char 'b'))) in
  let re = Re.(compile (seq [ a; opt b ])) in
  assert (Re.group_count re = 3);
  assert (Re.group_names re = [ "a", 1; "b", 2 ]);
  let check copy =
    assert (Re.group_count copy = Re.group_count re);
    assert (Re.group_names copy = Re.group_names re);
    List.iter
      (fun s ->
         let extract r =
           Option.map
             (fun g -> Re.Group.all_offset g, Re.Mark.Set.elements (Re.Mark.all g))
             (Re.exec_opt r s)
         in
         assert (extract copy = extract re))
      [ ""; "a"; "ab"; "xxab"; "b"; "aaa" ]
  in
  check (Re.copy_re re);
  check (Re.copy_re re);
  let g = Re.exec re "a" in
  assert (Re.Mark.Set.equal (Re.Mark.all g) (Re.Mark.Set.singleton mark_a));
  assert (not (Re.Mark.test g mark_b));
  let g = Re.exec re "ab" in
  assert (
    Re.Mark.Set.equal
      (Re.Mark.all g)
      (Re.Mark.Set.add mark_b (Re.Mark.Set.singleton mark_a)));
  assert (Re.Mark.equal mark_a mark_a && not (Re.Mark.equal mark_a mark_b));
  assert (Re.Mark.compare mark_a mark_b <> 0);
  assert (Re.group_count Re.(compile (no_group (group (char 'a')))) = 1);
  [%expect {| |}]
;;

let%expect_test "every group accessor distinguishes absent and empty groups" =
  let re =
    Re.(compile (seq [ group (char 'a'); opt (group (char 'b')); group epsilon ]))
  in
  let g = Re.exec re "xa" in
  assert (Re.Group.nb_groups g = 4);
  List.iter
    (fun (i, expected) ->
       assert (Re.Group.offset_opt g i = expected);
       assert (Re.Group.start_opt g i = Option.map fst expected);
       assert (Re.Group.stop_opt g i = Option.map snd expected);
       assert (Re.Group.test g i = Option.is_some expected);
       match expected with
       | None ->
         assert (Re.Group.get_opt g i = None);
         List.iter
           (fun f -> not_found f)
           [ (fun () -> ignore (Re.Group.get g i))
           ; (fun () -> ignore (Re.Group.start g i))
           ; (fun () -> ignore (Re.Group.stop g i))
           ; (fun () -> ignore (Re.Group.offset g i))
           ]
       | Some (start, stop) ->
         assert (Re.Group.start g i = start && Re.Group.stop g i = stop);
         assert (Re.Group.get g i = String.sub "xa" start (stop - start)))
    [ 0, Some (1, 2); 1, Some (1, 2); 2, None; 3, Some (2, 2); 4, None; 20, None ];
  [%expect {| |}]
;;

let%expect_test "character-set validation and witness wrappers" =
  let open Re in
  List.iter
    (fun f -> invalid f)
    [ (fun () -> inter [ char 'a'; str "ab" ])
    ; (fun () -> compl [ str "ab" ])
    ; (fun () -> diff (str "ab") (char 'a'))
    ; (fun () -> diff (char 'a') (str "ab"))
    ];
  let base = str "ab" in
  let _, marked = mark base in
  List.iter
    (fun r -> assert (execp (compile r) (witness r)))
    [ alt [ base; str "cd" ]
    ; group base
    ; no_group (group base)
    ; nest base
    ; marked
    ; first base
    ; shortest base
    ; longest base
    ; greedy base
    ; non_greedy base
    ; repn base 0 (Some 2)
    ; repn base 2 (Some 3)
    ];
  let r = compile (inter [ no_case (char 'a'); char 'A' ]) in
  assert (execp r "A" && not (execp r "a"));
  [%expect {| |}]
;;
