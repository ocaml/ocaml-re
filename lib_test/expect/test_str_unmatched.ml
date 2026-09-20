module Re = Re_private.Re

let%expect_test "replacement references to unmatched groups agree with Str" =
  let pattern = {|\(a\)?b|} in
  let replacement = {|\1|} in
  let compare input =
    let run name replace =
      match replace input with
      | text -> Printf.printf "%s on %S: %S\n" name input text
      | exception exn ->
        Printf.printf "%s on %S: %s\n" name input (Printexc.to_string exn)
    in
    run "Str" (Str.replace_first (Str.regexp pattern) replacement);
    run "Re.Str" (Re.Str.replace_first (Re.Str.regexp pattern) replacement)
  in
  compare "ab";
  compare "b";
  [%expect
    {|
    Str on "ab": "a"
    Re.Str on "ab": "a"
    Str on "b": Failure("Str.replace: reference to unmatched group")
    Re.Str on "b": Failure("Str.replace: reference to unmatched group")
    |}]
;;

let%expect_test "participating empty groups remain valid replacement references" =
  let pattern = {|\(a*\)b|} in
  let replacement = {|\1|} in
  Printf.printf "Str: %S\n" (Str.replace_first (Str.regexp pattern) replacement "b");
  Printf.printf
    "Re.Str: %S\n"
    (Re.Str.replace_first (Re.Str.regexp pattern) replacement "b");
  [%expect
    {|
    Str: ""
    Re.Str: ""
    |}]
;;
