module Re = Re_private.Re

let%expect_test "factoring composite prefixes preserves languages and captures" =
  let constructors =
    Re.
      [ (fun () -> str "ab"), [ "ab" ]
      ; (fun () -> repn (char 'a') 2 (Some 3)), [ "aa"; "aaa" ]
      ; (fun () -> nest (str "ab")), [ "ab" ]
      ; (fun () -> alt [ str "ab"; str "ac" ]), [ "ab"; "ac" ]
      ; (fun () -> first (str "ab")), [ "ab" ]
      ; (fun () -> longest (str "ab")), [ "ab" ]
      ; (fun () -> shortest (str "ab")), [ "ab" ]
      ; (fun () -> greedy (repn (char 'a') 1 (Some 2))), [ "a"; "aa" ]
      ; (fun () -> non_greedy (repn (char 'a') 1 (Some 2))), [ "a"; "aa" ]
      ; (fun () -> group (str "ab")), [ "ab" ]
      ]
  in
  List.iter
    (fun (make_prefix, accepted) ->
       let re =
         Re.(
           compile
             (whole_string
                (alt
                   [ seq [ make_prefix (); char 'x' ]; seq [ make_prefix (); char 'y' ] ])))
       in
       List.iter
         (fun prefix ->
            List.iter
              (fun suffix ->
                 let expected =
                   List.mem prefix accepted && List.mem suffix [ "x"; "y" ]
                 in
                 assert (Re.execp re (prefix ^ suffix) = expected))
              [ ""; "x"; "y"; "z" ])
         [ ""; "a"; "aa"; "aaa"; "aaaa"; "ab"; "ac"; "ad" ])
    constructors;
  (* Similar but unequal prefixes must not be merged. *)
  let re =
    Re.(
      compile
        (whole_string
           (alt
              [ seq [ repn (char 'a') 2 (Some 2); char 'x' ]
              ; seq [ repn (char 'a') 3 (Some 3); char 'y' ]
              ])))
  in
  assert (Re.execp re "aax" && Re.execp re "aaay");
  assert ((not (Re.execp re "aaax")) && not (Re.execp re "aay"));
  let re =
    Re.(
      compile
        (alt [ seq [ group (str "ab"); char 'x' ]; seq [ group (str "ab"); char 'y' ] ]))
  in
  assert (Re.Group.all_offset (Re.exec re "aby") = [| 0, 3; -1, -1; 0, 2 |]);
  let mark, prefix = Re.(mark (str "ab")) in
  let re = Re.(compile (alt [ seq [ prefix; char 'x' ]; seq [ prefix; char 'y' ] ])) in
  assert (Re.Mark.test (Re.exec re "aby") mark);
  let mark2, prefix2 = Re.(mark (str "ab")) in
  let re = Re.(compile (alt [ seq [ prefix; char 'x' ]; seq [ prefix2; char 'y' ] ])) in
  let g = Re.exec re "aby" in
  assert (Re.Mark.test g mark2 && not (Re.Mark.test g mark));
  [%expect {| |}]
;;

let%expect_test "last-newline assertions respect restricted windows" =
  let re = Re.(compile (seq [ char 'a'; leol ])) in
  assert (Re.execp ~pos:0 ~len:1 re "a\n");
  assert (Re.Group.offset (Re.exec ~pos:0 ~len:1 re "a\n") 0 = (0, 1));
  assert (not (Re.execp ~pos:0 ~len:1 re "ab"));
  let boundary = Re.compile Re.leol in
  assert (Re.execp ~pos:2 ~len:0 boundary "a\n");
  assert (Re.execp ~pos:1 ~len:0 boundary "a\n");
  [%expect {| |}]
;;
