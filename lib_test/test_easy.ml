(* Tests for Re higher-level functions *)

open OUnit

let pp_str x = x
let pp_list l = "[" ^ String.concat ", " l ^ "]"

let re_whitespace = Re_posix.compile_pat "[\t ]+"

let test_iter () =
  let re = Re_posix.compile_pat "(ab)+" in
  assert_equal ~printer:pp_list
    ["abab"; "ab"; "ab"] (Re.matches re "aabab aaabba  dab ");
  assert_equal ~printer:pp_list
    ["ab"; "abab"] (Re.matches ~pos:2 ~len:7 re "abab ababab");
  ()

let test_split () =
  assert_equal ~printer:pp_list
    ["aa"; "bb"; "c"; "d"] (Re.split re_whitespace "aa bb c d ");
  assert_equal ~printer:pp_list
    ["a"; "b"] (Re.split ~pos:1 ~len:4 re_whitespace "aa b c d");
  assert_equal ~printer:pp_list
    ["a"; "full_word"; "bc"] (Re.split re_whitespace " a full_word bc   ");
  ()

let test_replace () =
  let re = Re_posix.compile_pat "[a-zA-Z]+" in
  let f sub = String.capitalize (Re.get sub 0) in
  assert_equal ~printer:pp_str  " Hello World; I Love Chips!"
    (Re.replace re ~f " hello world; I love chips!");
  assert_equal ~printer:pp_str " Allo maman, bobo"
    (Re.replace ~all:false re ~f " allo maman, bobo");
  ()

let suite = "easy" >:::
  [ "iter" >:: test_iter
  ; "split" >:: test_split
  ; "replace" >:: test_replace
  ]

let () =
  ignore (run_test_tt_main suite)
