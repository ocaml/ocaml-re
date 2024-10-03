open Import
open Re

let%expect_test "str" =
  test_re (str "a") "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (str "a") "b";
  [%expect {| Not_found |}]
;;

let%expect_test "char" =
  test_re (alt [ char 'a'; char 'b' ]) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (alt [ char 'a'; char 'b' ]) "b";
  [%expect {| [| (0, 1) |] |}];
  test_re (alt [ char 'a'; char 'b' ]) "c";
  [%expect {| Not_found |}]
;;

let%expect_test "alt" =
  test_re (alt [ char 'a'; char 'b' ]) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (alt [ char 'a'; char 'b' ]) "b";
  [%expect {| [| (0, 1) |] |}];
  test_re (alt [ char 'a'; char 'b' ]) "c";
  [%expect {| Not_found |}]
;;

let%expect_test "seq" =
  test_re (seq [ char 'a'; char 'b' ]) "ab";
  [%expect {| [| (0, 2) |] |}];
  test_re (seq [ char 'a'; char 'b' ]) "ac";
  [%expect {| Not_found |}]
;;

let%expect_test "empty" =
  test_re empty "";
  [%expect {| Not_found |}];
  test_re empty "a";
  [%expect {| Not_found |}]
;;

let%expect_test "epsilon" =
  test_re epsilon "";
  [%expect {| [| (0, 0) |] |}];
  test_re epsilon "a";
  [%expect {| [| (0, 0) |] |}]
;;

let%expect_test "rep" =
  test_re (rep (char 'a')) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (rep (char 'a')) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (rep (char 'a')) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (rep (char 'a')) "b";
  [%expect {| [| (0, 0) |] |}]
;;

let%expect_test "bol" =
  test_re (seq [ bol; char 'a' ]) "ab";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ bol; char 'a' ]) "b\na";
  [%expect {| [| (2, 3) |] |}];
  test_re (seq [ bol; char 'a' ]) "ba";
  [%expect {| Not_found |}]
;;

let%expect_test "eol" =
  test_re (seq [ char 'a'; eol ]) "ba";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char 'a'; eol ]) "a\nb";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ char 'a'; eol ]) "ba\n";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char 'a'; eol ]) "ab";
  [%expect {| Not_found |}]
;;

let%expect_test "bow" =
  test_re (seq [ bow; char 'a' ]) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ bow; char 'a' ]) "bb aa";
  [%expect {| [| (3, 4) |] |}];
  test_re (seq [ bow; char 'a' ]) "ba ba";
  [%expect {| Not_found |}];
  test_re bow ";";
  [%expect {| Not_found |}];
  test_re bow "";
  [%expect {| Not_found |}]
;;

let%expect_test "eow" =
  test_re (seq [ char 'a'; eow ]) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ char 'a'; eow ]) "bb aa";
  [%expect {| [| (4, 5) |] |}];
  test_re (seq [ char 'a'; eow ]) "ab ab";
  [%expect {| Not_found |}];
  test_re eow ";";
  [%expect {| Not_found |}];
  test_re eow "";
  [%expect {| Not_found |}]
;;

let%expect_test "bos" =
  test_re (seq [ bos; char 'a' ]) "ab";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ bos; char 'a' ]) "b\na";
  [%expect {| Not_found |}];
  test_re (seq [ bos; char 'a' ]) "ba";
  [%expect {| Not_found |}]
;;

let%expect_test "eos" =
  test_re (seq [ char 'a'; eos ]) "ba";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char 'a'; eos ]) "a\nb";
  [%expect {| Not_found |}];
  test_re (seq [ char 'a'; eos ]) "ba\n";
  [%expect {| Not_found |}];
  test_re (seq [ char 'a'; eos ]) "ab";
  [%expect {| Not_found |}]
;;

let%expect_test "leol" =
  test_re (seq [ char 'a'; leol ]) "ba";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char 'a'; leol ]) "a\nb";
  [%expect {| Not_found |}];
  test_re (seq [ char 'a'; leol ]) "ba\n";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char 'a'; leol ]) "ab";
  [%expect {| Not_found |}];
  test_re (alt [ str "b\n"; seq [ char 'a'; leol ] ]) "ab\n";
  [%expect {| [| (1, 3) |] |}]
;;

let%expect_test "start" =
  test_re ~pos:1 (seq [ start; char 'a' ]) "xab";
  [%expect {| [| (1, 2) |] |}];
  test_re ~pos:1 (seq [ start; char 'a' ]) "xb\na";
  [%expect {| Not_found |}];
  test_re ~pos:1 (seq [ start; char 'a' ]) "xba";
  [%expect {| Not_found |}]
;;

let%expect_test "stop" =
  test_re ~len:2 (seq [ char 'a'; stop ]) "bax";
  [%expect {| [| (1, 2) |] |}];
  test_re ~len:3 (seq [ char 'a'; stop ]) "a\nbx";
  [%expect {| Not_found |}];
  test_re ~len:3 (seq [ char 'a'; stop ]) "ba\nx";
  [%expect {| Not_found |}];
  test_re ~len:2 (seq [ char 'a'; stop ]) "abx";
  [%expect {| Not_found |}]
;;

let%expect_test "word" =
  test_re (word (str "aa")) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (word (str "aa")) "bb aa";
  [%expect {| [| (3, 5) |] |}];
  test_re (word (str "aa")) "aaa";
  [%expect {| Not_found |}];
  test_re (word (str "")) "";
  [%expect {| Not_found |}]
;;

let%expect_test "not_boundary" =
  test_re (seq [ not_boundary; char 'b'; not_boundary ]) "abc";
  [%expect {| [| (1, 2) |] |}];
  test_re (seq [ char ';'; not_boundary; char ';' ]) ";;";
  [%expect {| [| (0, 2) |] |}];
  test_re (seq [ not_boundary; char ';'; not_boundary ]) ";";
  [%expect {| [| (0, 1) |] |}];
  test_re (seq [ not_boundary; char 'a' ]) "abc";
  [%expect {| Not_found |}];
  test_re (seq [ char 'c'; not_boundary ]) "abc";
  [%expect {| Not_found |}]
;;

let%expect_test "default match semantics" =
  test_re (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ]) "aabaab";
  [%expect {| [| (0, 6) |] |}];
  test_re (alt [ str "aa"; str "aaa" ]) "aaaa";
  [%expect {| [| (0, 2) |] |}];
  test_re (alt [ str "aaa"; str "aa" ]) "aaaa";
  [%expect {| [| (0, 3) |] |}]
;;

let%expect_test "shortest match" =
  test_re (shortest (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])) "aabaab";
  [%expect {| [| (0, 3) |] |}];
  test_re (shortest (alt [ str "aa"; str "aaa" ])) "aaaa";
  [%expect {| [| (0, 2) |] |}];
  test_re (shortest (alt [ str "aaa"; str "aa" ])) "aaaa";
  [%expect {| [| (0, 2) |] |}]
;;

let%expect_test "longest match" =
  test_re (longest (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])) "aabaab";
  [%expect {| [| (0, 6) |] |}];
  test_re (longest (alt [ str "aa"; str "aaa" ])) "aaaa";
  [%expect {| [| (0, 3) |] |}];
  test_re (longest (alt [ str "aaa"; str "aa" ])) "aaaa";
  [%expect {| [| (0, 3) |] |}]
;;

let%expect_test "first match" =
  test_re (first (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])) "aabaab";
  [%expect {| [| (0, 6) |] |}];
  test_re (first (alt [ str "aa"; str "aaa" ])) "aaaa";
  [%expect {| [| (0, 2) |] |}];
  test_re (first (alt [ str "aaa"; str "aa" ])) "aaaa";
  [%expect {| [| (0, 3) |] |}]
;;

let%expect_test "match_semantics" =
  let r = rep (group (alt [ str "aaa"; str "aa" ])) in
  test_re (longest r) "aaaaaaa";
  [%expect {| [| (0, 7); (5, 7) |] |}];
  test_re (first r) "aaaaaaa";
  [%expect {| [| (0, 6); (3, 6) |] |}];
  test_re (first (non_greedy r)) "aaaaaaa";
  [%expect {| [| (0, 0); (-1, -1) |] |}];
  test_re (shortest r) "aaaaaaa";
  [%expect {| [| (0, 0); (-1, -1) |] |}];
  let r' = rep (group (shortest (alt [ str "aaa"; str "aa" ]))) in
  test_re (longest r') "aaaaaaa";
  [%expect {| [| (0, 7); (4, 7) |] |}];
  test_re (first r') "aaaaaaa";
  [%expect {| [| (0, 6); (4, 6) |] |}]
;;

let%expect_test "greedy" =
  test_re (greedy (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])) "aabaab";
  [%expect {| [| (0, 6) |] |}];
  test_re (greedy (rep (group (opt (char 'a'))))) "aa";
  [%expect {| [| (0, 2); (2, 2) |] |}]
;;

let%expect_test "non_greedy" =
  test_re
    (non_greedy (longest (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])))
    "aabaab";
  [%expect {| [| (0, 6) |] |}];
  test_re
    (non_greedy (first (seq [ rep (alt [ char 'a'; char 'b' ]); char 'b' ])))
    "aabaab";
  [%expect {| [| (0, 3) |] |}];
  test_re (non_greedy (longest (rep (group (opt (char 'a')))))) "aa";
  [%expect {| [| (0, 2); (1, 2) |] |}]
;;

let%expect_test "set" =
  test_re (rep1 (set "abcd")) "bcbadbabcdba";
  [%expect {| [| (0, 12) |] |}];
  test_re (set "abcd") "e";
  [%expect {| Not_found |}]
;;

let%expect_test "rg" =
  test_re (rep1 (rg '0' '9')) "0123456789";
  [%expect {| [| (0, 10) |] |}];
  test_re (rep1 (rg '0' '9')) "a";
  [%expect {| Not_found |}]
;;

let%expect_test "inter" =
  test_re (rep1 (inter [ rg '0' '9'; rg '4' '6' ])) "456";
  [%expect {| [| (0, 3) |] |}];
  test_re (rep1 (inter [ rg '0' '9'; rg '4' '6' ])) "7";
  [%expect {| Not_found |}];
  test_re (inter [ alt [ char 'a'; char 'b' ]; char 'b' ]) "b";
  [%expect {| [| (0, 1) |] |}]
;;

let%expect_test "diff" =
  test_re (rep1 (diff (rg '0' '9') (rg '4' '6'))) "0123789";
  [%expect {| [| (0, 7) |] |}];
  test_re (rep1 (diff (rg '0' '9') (rg '4' '6'))) "4";
  [%expect {| Not_found |}]
;;

let%expect_test "compl" =
  test_re (rep1 (compl [ rg '0' '9'; rg 'a' 'z' ])) "A:Z+";
  [%expect {| [| (0, 4) |] |}];
  test_re (rep1 (compl [ rg '0' '9'; rg 'a' 'z' ])) "0";
  [%expect {| Not_found |}];
  test_re (rep1 (compl [ rg '0' '9'; rg 'a' 'z' ])) "a";
  [%expect {| Not_found |}]
;;

let%expect_test "case" =
  test_re (case (str "abc")) "abc";
  [%expect {| [| (0, 3) |] |}];
  test_re (no_case (case (str "abc"))) "abc";
  [%expect {| [| (0, 3) |] |}];
  test_re (case (str "abc")) "ABC";
  [%expect {| Not_found |}];
  test_re (no_case (case (str "abc"))) "ABC";
  [%expect {| Not_found |}]
;;

let%expect_test "no_case" =
  test_re (no_case (str "abc")) "abc";
  [%expect {| [| (0, 3) |] |}];
  test_re (no_case (str "abc")) "ABC";
  [%expect {| [| (0, 3) |] |}];
  test_re (case (no_case (str "abc"))) "abc";
  [%expect {| [| (0, 3) |] |}];
  test_re (case (no_case (str "abc"))) "ABC";
  [%expect {| [| (0, 3) |] |}]
;;

let%expect_test "witness" =
  let t re = print_endline (witness re) in
  t (set "ac");
  [%expect {| a |}];
  t (repn (str "foo") 3 None);
  [%expect {| foofoofoo |}];
  t (alt [ char 'c'; char 'd' ]);
  [%expect {| c |}];
  t (no_case (str "test"));
  [%expect {| TEST |}];
  t eol;
  [%expect {| |}]
;;
