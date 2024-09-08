open Import

let t re s =
  let group = Re.exec_opt (Re.compile re) s in
  Format.printf "%a@." (Fmt.opt Re.Group.pp) group
;;

let%expect_test "fixed repetition" =
  let re = Re.compile @@ Re.(repn (char 'a') 3 (Some 3)) in
  let test s = printf "%b\n" (Re.execp re s) in
  test "";
  [%expect {| false |}];
  test "aa";
  [%expect {| false |}];
  test "aaa";
  [%expect {| true |}];
  test "aaaa";
  [%expect {| true |}]
;;

open Re

let%expect_test "empty group" =
  let empty = group empty in
  t empty "";
  [%expect {| <None> |}];
  t empty "x";
  [%expect {| <None> |}]
;;

let%expect_test "zero length group" =
  let empty = group bos in
  t empty "";
  [%expect {| (Group ( (0 0))( (0 0))) |}];
  t empty "x";
  [%expect {| (Group ( (0 0))( (0 0))) |}]
;;

let%expect_test "no group" =
  let re = any in
  t re "";
  [%expect {| <None> |}];
  t re ".";
  [%expect {| (Group (. (0 1))) |}]
;;

let%expect_test "two groups" =
  let re = seq [ group any; group any ] in
  t re "a";
  [%expect {| <None> |}];
  t re "ab";
  [%expect {| (Group (ab (0 2))(a (0 1))(b (1 2))) |}];
  t re "abc";
  [%expect {| (Group (ab (0 2))(a (0 1))(b (1 2))) |}]
;;

let%expect_test "maybe group" =
  let twoany = seq [ any; any ] in
  let re = alt [ twoany; group twoany ] in
  t re "aa";
  [%expect {| (Group (aa (0 2))( (-1 -1))) |}];
  t re "a";
  [%expect {| <None> |}]
;;

let%expect_test "nesting of groups" =
  let re = group (seq [ group (char 'a'); char 'b' ]) in
  t re "ab";
  [%expect {| (Group (ab (0 2))(ab (0 2))(a (0 1))) |}]
;;

open Re

let offset fmt (x, y) = Format.fprintf fmt "(%d, %d)" x y

let or_not_found f fmt v =
  match v () with
  | exception Not_found -> Format.fprintf fmt "Not_found"
  | s -> f fmt s
;;

let array f fmt v =
  Format.fprintf fmt "[| %a |]" (Fmt.list ~pp_sep:(Fmt.lit "; ") f) (Array.to_list v)
;;

let%expect_test "Group.{get,get_opt,offset,test}" =
  let r = seq [ group (char 'a'); opt (group (char 'a')); group (char 'b') ] in
  let m = exec (compile r) "ab" in
  let test idx =
    Format.printf "get_opt = %a@." (Fmt.opt Fmt.str) (Group.get_opt m idx);
    Format.printf "get = %a@." (or_not_found Fmt.str) (fun () -> Group.get m idx);
    Format.printf "test = %b@." (Group.test m idx);
    Format.printf "offset = %a@." (or_not_found offset) (fun () -> Group.offset m idx)
  in
  test 0;
  [%expect {|
    get_opt = ab
    get = ab
    test = true
    offset = (0, 2) |}];
  test 1;
  [%expect {|
    get_opt = a
    get = a
    test = true
    offset = (0, 1) |}];
  test 2;
  [%expect
    {|
    get_opt = <None>
    get = Not_found
    test = false
    offset = Not_found |}];
  test 3;
  [%expect {|
    get_opt = b
    get = b
    test = true
    offset = (1, 2) |}];
  Format.printf "%a@." (array offset) (Group.all_offset m);
  [%expect {| [| (0, 2); (0, 1); (-1, -1); (1, 2) |] |}]
;;

let test_re ?pos ?len r s =
  let offsets () = Group.all_offset (Re.exec ?pos ?len (Re.compile r) s) in
  Format.printf "%a@." (or_not_found (array offset)) offsets
;;

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

let%expect_test "rep1" =
  test_re (rep1 (char 'a')) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (rep1 (char 'a')) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (rep1 (char 'a')) "";
  [%expect {| Not_found |}];
  test_re (rep1 (char 'a')) "b";
  [%expect {| Not_found |}]
;;

let%expect_test "repn" =
  test_re (repn (char 'a') 0 None) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (repn (char 'a') 0 (Some 0)) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (repn (char 'a') 1 (Some 2)) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (repn (char 'a') 1 (Some 2)) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (repn (char 'a') 1 (Some 2)) "";
  [%expect {| Not_found |}];
  test_re (repn (char 'a') 1 (Some 2)) "aaa";
  [%expect {| [| (0, 2) |] |}];
  invalid_argument (fun () -> repn empty (-1) None);
  [%expect {| Invalid_argument "Re.repn" |}];
  invalid_argument (fun () -> repn empty 1 (Some 0));
  [%expect {| Invalid_argument "Re.repn" |}]
;;

let%expect_test "opt" =
  test_re (opt (char 'a')) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (opt (char 'a')) "a";
  [%expect {| [| (0, 1) |] |}]
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

let%expect_test "nest" =
  let r = rep (nest (alt [ group (char 'a'); char 'b' ])) in
  test_re r "ab";
  [%expect {| [| (0, 2); (-1, -1) |] |}];
  test_re r "ba";
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

let%expect_test "group/no_group" =
  let r = seq [ group (char 'a'); opt (group (char 'a')); group (char 'b') ] in
  test_re r "ab";
  [%expect {| [| (0, 2); (0, 1); (-1, -1); (1, 2) |] |}];
  test_re (no_group r) "ab";
  [%expect {| [| (0, 2) |] |}]
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

let%expect_test "group choice" =
  let t = Import.exec_partial_detailed in
  (* Alternation of character sets isn't flattened *)
  let lhs_group =
    let open Re in
    alt [ group (char 'a'); char 'b' ]
  in
  t lhs_group "a";
  [%expect {| `Full [|0,1;0,1|] |}];
  t lhs_group "b";
  [%expect {| `Full [|0,1;-1,-1|] |}];
  t
    (let open Re in
     alt [ group (char 'a'); group (char 'b') ])
    "b";
  [%expect {| `Full [|0,1;-1,-1;0,1|] |}];
  (* No_group inside char set: *)
  let no_group_charset =
    let a = Re.group (Re.char 'a') in
    let b = Re.char 'b' in
    Re.no_group (Re.alt [ a; b ])
  in
  t no_group_charset "a";
  [%expect {| `Full [|0,1|] |}];
  t no_group_charset "b";
  [%expect {| `Full [|0,1|] |}];
  (* No_group outside char set *)
  let no_group_string =
    let aa = Re.group (Re.str "aa") in
    let bb = Re.str "bb" in
    Re.no_group (Re.alt [ aa; bb ])
  in
  t no_group_string "aa";
  [%expect {| `Full [|0,2|] |}];
  t no_group_string "bb";
  [%expect {| `Full [|0,2|] |}]
;;
