open Import
open Re

let%expect_test "no_group is scoped and preserves remaining names and numbering" =
  let r =
    seq
      [ group ~name:"before" (char 'x')
      ; no_group (Perl.re "(?<ignored>a(b))|(ac)")
      ; group ~name:"after" (char 'y')
      ]
  in
  let compiled = compile r in
  assert (group_count compiled = 3);
  assert (List.equal ~eq:Poly.equal (group_names compiled) [ "before", 1; "after", 2 ]);
  test_re r "xaby";
  test_re r "xacy";
  [%expect
    {|
    [| (0, 4); (0, 1); (3, 4) |]
    [| (0, 4); (0, 1); (3, 4) |]
    |}]
;;

let%expect_test "ignored captures preserve case scopes and nest" =
  let r =
    seq [ group (char 'a'); case (group (char 'B')); no_group (group (char 'c')) ]
    |> no_case
    |> nest
    |> no_group
    |> group
  in
  test_re r "ABC";
  test_re r "AbC";
  [%expect
    {|
    [| (0, 3); (0, 3) |]
    Not_found
    |}]
;;

let%expect_test "no_group preserves persistent marks and alternative preference" =
  let left, a = mark (group (str "ab")) in
  let right, b = mark (group (str "abc")) in
  let re = no_group (alt [ a; b ]) |> compile in
  let result = exec re "abc" in
  assert (Mark.test result left);
  assert (not (Mark.test result right));
  assert (Group.stop result 0 = 2);
  assert (group_count re = 1);
  [%expect {||}]
;;

let%expect_test "ignored groups do not change preference through variable prefixes" =
  List.iter [ "(a*?b)|(a*?a)"; "(a*a)|(a*b)" ] ~f:(fun pattern ->
    let r = Perl.re pattern |> no_group in
    test_re r "aab");
  [%expect
    {|
    [| (0, 3) |]
    [| (0, 2) |]
    |}]
;;

let%expect_test "assertion prefixes inside ignored groups preserve spans and preference" =
  let alternatives assertion =
    no_group
      (alt [ group (seq [ assertion; str "ab" ]); group (seq [ assertion; str "abc" ]) ])
  in
  test_re (alternatives bos) "abc";
  test_re (alternatives bos) "xabc";
  test_re (alternatives bol) "ab";
  test_re (alternatives bol) "x\nabc";
  test_re (alternatives bow) "abc";
  test_re (alternatives bow) "x abc";
  test_re (alternatives not_boundary) "xabc";
  test_re (alternatives not_boundary) "abc";
  test_re (alternatives start) "abc";
  test_re (alternatives start) "xabc";
  test_re ~pos:1 (alternatives start) "xabc";
  [%expect
    {|
    [| (0, 2) |]
    Not_found
    [| (0, 2) |]
    [| (2, 4) |]
    [| (0, 2) |]
    [| (2, 4) |]
    [| (1, 3) |]
    Not_found
    [| (0, 2) |]
    Not_found
    [| (1, 3) |]
    |}]
;;

let%expect_test "erasing captures preserves boolean results and overall match spans" =
  let rec strings n =
    if n = 0
    then [ "" ]
    else
      ""
      :: List.concat_map
           (strings (n - 1))
           ~f:(fun s -> List.map [ "a"; "b"; "A"; "/" ] ~f:(fun c -> c ^ s))
  in
  let inputs = strings 4 in
  let patterns =
    [ "(a)|(ab)|(aa)"
    ; "(ab)|(a)|(aa)"
    ; "()|(a)|(ab)"
    ; "((a)|(ab))*"
    ; "((a*?b)|(a*))"
    ; "((a)|(ab)){1,3}"
    ; "(^a[^/]*\\.b$)|(^a[^/]*\\.a$)"
    ; "(a(b))|(a(a))"
    ; "(a*?b)|(a*?a)"
    ; "(a*a)|(a*b)"
    ; "((a|aa)b)|((a|aa)a)"
    ; "(aa*?b)|(aa*?a)"
    ; "(aa*a)|(aa*b)"
    ]
  in
  List.iter patterns ~f:(fun pattern ->
    List.iter
      [ (fun x -> x); longest; shortest; no_case ]
      ~f:(fun semantics ->
        let ast = semantics (Perl.re pattern) in
        let captures = compile ast in
        let ignored = compile (no_group ast) in
        List.iter inputs ~f:(fun input ->
          let span re =
            Option.map (fun groups -> Group.offset groups 0) (exec_opt re input)
          in
          assert (Option.equal Poly.equal (span captures) (span ignored));
          assert (Bool.equal (execp captures input) (execp ignored input));
          assert (Poly.equal (exec_partial captures input) (exec_partial ignored input)))));
  [%expect {||}]
;;
