open Import

let matched f =
  try Some (f ()) with
  | Not_found -> None
;;

let%test_unit "partial-match state and captures agree with Str" =
  (* The original audit used native Str, not the JavaScript emulation. *)
  match Sys.backend_type with
  | Other _ -> ()
  | Native | Bytecode ->
    List.iter
      [ "abc", "ab", 0
      ; {|\(a\)\(bc\)|}, "ab", 2
      ; {|\(ab\)c|}, "ab", 1
      ; {|\(abc\)|}, "ab", 1
      ; {|a\(b?\)c|}, "a", 1
      ; {|\(ab\)\|\(a\)|}, "a", 2
      ; {|\(a\)$|}, "a", 1
      ; {|\(abc\)*|}, "abcab", 1
      ; "abc", "x", 0
      ; "abc", "", 0
      ]
      ~f:(fun (pattern, text, groups) ->
        for pos = 0 to 1 do
          let text = String.make pos '_' ^ text in
          let expected = Str.string_partial_match (Str.regexp pattern) text pos in
          assert (
            Bool.equal
              expected
              (Re.Str.string_partial_match (Re.Str.regexp pattern) text pos));
          if expected
          then (
            assert (Str.match_beginning () = Re.Str.match_beginning ());
            assert (Str.match_end () = Re.Str.match_end ());
            for i = 0 to groups do
              assert (
                Option.equal
                  String.equal
                  (matched (fun () -> Str.matched_group i text))
                  (matched (fun () -> Re.Str.matched_group i text)))
            done)
        done)
;;
