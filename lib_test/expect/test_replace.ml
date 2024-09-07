open Import

let%expect_test "test_replace" =
  let re = Re.Posix.compile_pat "[a-zA-Z]+" in
  let f sub = String.capitalize_ascii (Re.Group.get sub 0) in
  print_endline (Re.replace re ~f " hello world; I love chips!");
  [%expect {| Hello World; I Love Chips! |}];
  print_endline (Re.replace ~all:false re ~f " allo maman, bobo");
  [%expect {| Allo maman, bobo |}];
  print_endline (Re.replace re_empty ~f:(fun _ -> "a") "");
  [%expect {| a |}];
  print_endline (Re.replace (Re.compile (Re.rep (Re.char 'a'))) ~f:(fun _ -> "*") "cat");
  [%expect {| *c*t* |}]
;;

let%expect_test "test_replace_string" =
  let re = Re.Posix.compile_pat "_[a-zA-Z]+_" in
  print_endline (Re.replace_string re ~by:"goodbye" "_hello_ world");
  [%expect {| goodbye world |}];
  print_endline (Re.replace_string ~all:false re ~by:"brown" "The quick _XXX_ fox");
  [%expect {| The quick brown fox |}]
;;

let%expect_test "test_bug_55" =
  let re = Re.(compile bol) in
  let res = Re.replace_string re ~by:"z" "abc" in
  print_endline res;
  [%expect {| zabc |}];
  let re = Re.(compile eow) in
  let res = Re.replace_string re ~by:"X" "one two three" in
  print_endline res;
  [%expect {| oneX twoX threeX |}]
;;
