(* Tests for Re higher-level functions *)

open OUnit

let pp_str x = x
let pp_list l = "[" ^ String.concat ", " l ^ "]"

let re_whitespace = Re_posix.compile_pat "[\t ]+"
let re_empty = Re_posix.compile_pat ""

let test_iter () =
  let re = Re_posix.compile_pat "(ab)+" in
  assert_equal ~printer:pp_list
    ["abab"; "ab"; "ab"] (Re.matches re "aabab aaabba  dab ");
  assert_equal ~printer:pp_list
    ["ab"; "abab"] (Re.matches ~pos:2 ~len:7 re "abab ababab");
  assert_equal ~printer:pp_list
    [""; ""] (Re.matches re_empty "ab");
  ()

let test_split () =
  assert_equal ~printer:pp_list
    ["aa"; "bb"; "c"; "d"] (Re.split re_whitespace "aa bb c d ");
  assert_equal ~printer:pp_list
    ["a"; "b"] (Re.split ~pos:1 ~len:4 re_whitespace "aa b c d");
  assert_equal ~printer:pp_list
    ["a"; "full_word"; "bc"] (Re.split re_whitespace " a full_word bc   ");
  assert_equal ~printer:pp_list
    ["a"; "b"; "c"; "d"] (Re.split re_empty "abcd");
  ()

let map_split_delim =
  List.map
    (function
      | `Text x -> `T x
      | `Delim s -> `D (Re.get s 0)
    )

let pp_list' l =
  pp_list
    (List.map
      (function `T s -> s
      | `D s -> "delim '" ^ s ^ "'"
      ) l
    )

let (|>) x f = f x

let test_split_full () =
  assert_equal ~printer:pp_list'
    [`T "aa"; `D " "; `T "bb"; `D " "; `T "c"; `D " "; `T "d"; `D " "]
    (Re.split_full re_whitespace "aa bb c d " |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`T "a"; `D " \t"; `T "b"; `D " "]
    (Re.split_full ~pos:1 ~len:5 re_whitespace "aa \tb c d" |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`D " "; `T "a"; `D " "; `T "full_word"; `D " "; `T "bc"; `D "   "]
    (Re.split_full re_whitespace " a full_word bc   " |> map_split_delim);
  assert_equal ~printer:pp_list'
    [`D ""; `T "a"; `D ""; `T  "b"] (* XXX: not trivial *)
    (Re.split_full re_empty "ab" |> map_split_delim);
  ()

let test_replace () =
  let re = Re_posix.compile_pat "[a-zA-Z]+" in
  let f sub = String.capitalize (Re.get sub 0) in
  assert_equal ~printer:pp_str  " Hello World; I Love Chips!"
    (Re.replace re ~f " hello world; I love chips!");
  assert_equal ~printer:pp_str " Allo maman, bobo"
    (Re.replace ~all:false re ~f " allo maman, bobo");
  ()

let lsplit2 () =
  let re = Re.compile (Re.str "-") in
  assert_equal (Some ("test", "123")) (Re.lsplit2 re "test-123")

let suite = "easy" >:::
  [ "iter" >:: test_iter
  ; "split" >:: test_split
  ; "split_full" >:: test_split_full
  ; "replace" >:: test_replace
  ; "lsplit2" >:: lsplit2
  ]

let () =
  ignore (run_test_tt_main suite)
