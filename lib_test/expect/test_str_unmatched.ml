module Re = Re_private.Re

let%expect_test "replacement references to unmatched groups disagree with Str" =
  let pattern = {|\(a\)?b|} in
  let replacement = {|\1|} in
  let compare input =
    let run name replace =
      match replace input with
      | text -> Printf.printf "%s on %S: %S\n" name input text
      | exception Failure _ -> Printf.printf "%s on %S: Failure\n" name input
    in
    run "Str" (Str.replace_first (Str.regexp pattern) replacement);
    run "Re.Str" (Re.Str.replace_first (Re.Str.regexp pattern) replacement)
  in
  compare "ab";
  compare "b";
  (* Re.Str should reject the reference when group 1 did not participate,
     as Str does. Record the current empty-string substitution. *)
  [%expect
    {|
    Str on "ab": "a"
    Re.Str on "ab": "a"
    Str on "b": Failure
    Re.Str on "b": ""
    |}]
;;
