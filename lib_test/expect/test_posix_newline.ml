module Re = Re_private.Re

let%expect_test "POSIX negated classes exclude newline even without Newline" =
  List.iter
    (fun (name, opts) ->
       let re = Re.(compile (whole_string (Posix.re ~opts "[^a]"))) in
       List.iter
         (fun input ->
            let result =
              match Re.exec_opt re input with
              | None -> "no match"
              | Some group -> Printf.sprintf "match %S" (Re.Group.get group 0)
            in
            Printf.printf "%s on %S: %s\n" name input result)
         [ "a"; "b"; "\n" ])
    [ "default", []; "Newline", [ `Newline ] ];
  (* Without the Newline option, [^a] should match newline. With Newline,
     it should exclude newline. The other inputs are unaffected controls. *)
  [%expect
    {|
    default on "a": no match
    default on "b": match "b"
    default on "\n": no match
    Newline on "a": no match
    Newline on "b": match "b"
    Newline on "\n": no match
    |}]
;;
