open Import

let re_whitespace = Re.Posix.compile_pat "[\t ]+"
let re_eol = Re.compile Re.eol
let re_bow = Re.compile Re.bow
let re_eow = Re.compile Re.eow

let%expect_test "split" =
  let split ?pos ?len re s = strings (Re.split ?pos ?len re s) in
  split re_whitespace "aa bb c d ";
  [%expect {| ["aa"; "bb"; "c"; "d"] |}];
  split ~pos:1 ~len:4 re_whitespace "aa b c d";
  [%expect {| ["a"; "b"] |}];
  split re_whitespace " a full_word bc   ";
  [%expect {| ["a"; "full_word"; "bc"] |}];
  split re_empty "abcd";
  [%expect {| ["a"; "b"; "c"; "d"] |}];
  split re_eol "a\nb";
  [%expect {|
    ["a"; "\nb"] |}];
  split re_bow "a b";
  [%expect {| ["a "; "b"] |}];
  split re_eow "a b";
  [%expect {| ["a"; " b"] |}];
  split re_whitespace "";
  [%expect {| [] |}];
  split re_empty "";
  [%expect {| [] |}]
;;

let%expect_test "split_delim" =
  let split_delim ?pos ?len re s = strings (Re.split_delim ?pos ?len re s) in
  split_delim re_whitespace "aa bb c d ";
  [%expect {| ["aa"; "bb"; "c"; "d"; ""] |}];
  split_delim ~pos:1 ~len:4 re_whitespace "aa b c d";
  [%expect {| ["a"; "b"; ""] |}];
  split_delim re_whitespace " a full_word bc   ";
  [%expect {| [""; "a"; "full_word"; "bc"; ""] |}];
  split_delim re_empty "abcd";
  [%expect {| [""; "a"; "b"; "c"; "d"; ""] |}];
  split_delim re_eol "a\nb";
  [%expect {| ["a"; "\nb"; ""] |}];
  split_delim re_bow "a b";
  [%expect {| [""; "a "; "b"] |}];
  split_delim re_eow "a b";
  [%expect {| ["a"; " b"; ""] |}];
  split_delim re_whitespace "";
  [%expect {| [""] |}];
  split_delim re_empty "";
  [%expect {| [""; ""] |}]
;;

let%expect_test "split_full" =
  let split_full ?pos ?len re s =
    let res = Re.split_full ?pos ?len re s in
    Format.printf
      "[%a]@."
      Fmt.(
        list ~pp_sep:(Fmt.lit "; ") (fun fmt what ->
          match what with
          | `Text s -> Format.fprintf fmt "`T %S" s
          | `Delim s -> Format.fprintf fmt "`D %S" (Re.Group.get s 0)))
      res
  in
  split_full re_whitespace "aa bb c d ";
  [%expect {| [`T "aa"; `D " "; `T "bb"; `D " "; `T "c"; `D " "; `T "d"; `D " "] |}];
  split_full ~pos:1 ~len:5 re_whitespace "aa \tb c d";
  [%expect {| [`T "a"; `D " \t"; `T "b"; `D " "] |}];
  split_full re_whitespace " a full_word bc   ";
  [%expect {| [`D " "; `T "a"; `D " "; `T "full_word"; `D " "; `T "bc"; `D "   "] |}];
  split_full re_empty "ab";
  [%expect {| [`D ""; `T "a"; `D ""; `T "b"; `D ""] |}];
  split_full Re.(compile (rep (char 'a'))) "cat";
  [%expect {| [`D ""; `T "c"; `D "a"; `T "t"; `D ""] |}];
  ()
;;
