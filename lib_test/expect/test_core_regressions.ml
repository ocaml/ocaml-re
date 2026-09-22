open Import

let%expect_test "zero-count repetitions retain named capture declarations" =
  let re = Re.Perl.compile_pat "(?<absent>a){0}(?<present>b)" in
  List.iter (Re.group_names re) ~f:(fun (name, index) -> printf "%S: %d\n" name index);
  [%expect {| "present": 1 |}]
;;

let%expect_test "execution lengths must not overflow the bounds check" =
  invalid_argument (fun () ->
    printf "%b\n" (Re.execp ~pos:1 ~len:max_int (Re.compile Re.epsilon) "a"));
  [%expect {| true |}]
;;

let witness re =
  match Re.witness re with
  | s -> printf "%S (matches: %b)\n" s (Re.execp (Re.compile re) s)
  | exception Assert_failure _ -> print_endline "Assert_failure"
;;

let%expect_test "witness skips empty-language alternatives" =
  witness Re.(alt [ empty; str "a" ]);
  [%expect {| Assert_failure |}]
;;

let%expect_test "repeating the empty language has the empty witness" =
  witness Re.(rep empty);
  [%expect {| Assert_failure |}]
;;
