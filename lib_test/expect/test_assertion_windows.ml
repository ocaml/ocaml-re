open Import
module Re = Re_private.Re

let%expect_test "boundary assertions agree with a model across bytes and windows" =
  let wordc = Re.compile Re.wordc in
  let words = Array.init 256 (fun c -> Re.execp wordc (String.make 1 (Char.chr c))) in
  let is_word s i = i >= 0 && i < String.length s && words.(Char.code s.[i]) in
  let at_bol s i = i = 0 || Char.equal s.[i - 1] '\n' in
  let at_eol s i = i = String.length s || Char.equal s.[i] '\n' in
  let at_leol s i =
    i = String.length s || (i = String.length s - 1 && Char.equal s.[i] '\n')
  in
  let non_boundary s i = Bool.equal (is_word s (i - 1)) (is_word s i) in
  let cases =
    Re.
      [ ("bol", bol, fun s _ _ i -> at_bol s i)
      ; ("eol", eol, fun s _ _ i -> at_eol s i)
      ; ("bos", bos, fun _ _ _ i -> i = 0)
      ; ("eos", eos, fun s _ _ i -> i = String.length s)
      ; ("bow", bow, fun s _ _ i -> (not (is_word s (i - 1))) && is_word s i)
      ; ("eow", eow, fun s _ _ i -> is_word s (i - 1) && not (is_word s i))
      ; ("not_boundary", not_boundary, fun s _ _ i -> non_boundary s i)
      ; ("leol", leol, fun s _ _ i -> at_leol s i)
      ; ("start", start, fun _ first _ i -> i = first)
      ; ("stop", stop, fun _ _ last i -> i = last)
      ; ("empty line", seq [ bol; eol ], fun s _ _ i -> at_bol s i && at_eol s i)
      ; ("word boundary", alt [ bow; eow ], fun s _ _ i -> not (non_boundary s i))
      ]
  in
  List.iter cases ~f:(fun (name, assertion, expected) ->
    List.iter [ false; true ] ~f:(fun consume ->
      let pattern =
        if consume then Re.seq [ Re.group Re.any; assertion ] else Re.group assertion
      in
      let exercise re =
        List.iter [ ""; "a"; "!"; "\n" ] ~f:(fun prefix ->
          List.iter [ ""; "a"; "!"; "\n"; "\na" ] ~f:(fun suffix ->
            for byte = 0 to 255 do
              let s = prefix ^ String.make 1 (Char.chr byte) ^ suffix in
              let first = String.length prefix in
              let check pos len =
                let last = pos + len in
                let want = expected s pos last last in
                let actual = Re.exec_opt ~pos ~len re s in
                if Option.is_some actual <> want || Re.execp ~pos ~len re s <> want
                then
                  failwith
                    (Printf.sprintf
                       "%s consume=%b input=%S pos=%d len=%d"
                       name
                       consume
                       s
                       pos
                       len);
                match actual with
                | None -> ()
                | Some g ->
                  assert (Re.Group.nb_groups g = 2);
                  for i = 0 to 1 do
                    assert (Poly.equal (Re.Group.offset g i) (pos, last))
                  done
              in
              if consume
              then check first 1
              else (
                check first 0;
                check (first + 1) 0)
            done))
      in
      let re = Re.compile (Re.longest pattern) in
      exercise re;
      exercise re;
      exercise (Re.copy_re re)));
  [%expect {| |}]
;;
