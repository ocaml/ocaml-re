open Import
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

let%expect_test "group choice" =
  let t = Import.exec_partial_detailed in
  (* Alternation of character sets isn't flattened *)
  let lhs_group =
    let open Re in
    alt [ group (char 'a'); char 'b' ]
  in
  t lhs_group "a";
  [%expect {| `Full [|0,1,"a";0,1,"a"|] |}];
  t lhs_group "b";
  [%expect {| `Full [|0,1,"b";-1,-1,<No match>|] |}];
  t
    (let open Re in
     alt [ group (char 'a'); group (char 'b') ])
    "b";
  [%expect {| `Full [|0,1,"b";-1,-1,<No match>;0,1,"b"|] |}];
  (* No_group inside char set: *)
  let no_group_charset =
    let a = Re.group (Re.char 'a') in
    let b = Re.char 'b' in
    Re.no_group (Re.alt [ a; b ])
  in
  t no_group_charset "a";
  [%expect {| `Full [|0,1,"a"|] |}];
  t no_group_charset "b";
  [%expect {| `Full [|0,1,"b"|] |}];
  (* No_group outside char set *)
  let no_group_string =
    let aa = Re.group (Re.str "aa") in
    let bb = Re.str "bb" in
    Re.no_group (Re.alt [ aa; bb ])
  in
  t no_group_string "aa";
  [%expect {| `Full [|0,2,"aa"|] |}];
  t no_group_string "bb";
  [%expect {| `Full [|0,2,"bb"|] |}]
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

let%expect_test "nest" =
  let r = rep (nest (alt [ group (char 'a'); char 'b' ])) in
  test_re r "ab";
  [%expect {| [| (0, 2); (-1, -1) |] |}];
  test_re r "ba";
  [%expect {| [| (0, 2); (1, 2) |] |}]
;;

let%expect_test "group/no_group" =
  let r = seq [ group (char 'a'); opt (group (char 'a')); group (char 'b') ] in
  test_re r "ab";
  [%expect {| [| (0, 2); (0, 1); (-1, -1); (1, 2) |] |}];
  test_re (no_group r) "ab";
  [%expect {| [| (0, 2) |] |}]
;;
