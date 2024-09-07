open Import

let%expect_test "iter" =
  let re = Re.Posix.compile_pat "(ab)+" in
  strings (Re.matches re "aabab aaabba  dab ");
  [%expect {| ["abab"; "ab"; "ab"] |}];
  strings (Re.matches ~pos:2 ~len:7 re "abab ababab");
  [%expect {| ["ab"; "abab"] |}];
  strings (Re.matches re_empty "ab");
  [%expect {| [""; ""; ""] |}];
  strings (Re.matches (Re.compile (Re.rep (Re.char 'a'))) "cat");
  [%expect {| [""; "a"; ""] |}]
;;
