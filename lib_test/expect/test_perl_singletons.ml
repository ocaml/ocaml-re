open Import
open Re

let same_ast pattern expected =
  let show r = Format.asprintf "%a" pp r in
  assert (String.equal (show (Perl.re pattern)) (show expected))
;;

let%expect_test "empty branches preserve their AST" =
  same_ast "" epsilon;
  same_ast "()" (group epsilon);
  same_ast "|a||bc|" (alt [ epsilon; char 'a'; epsilon; str "bc"; epsilon ]);
  same_ast "(a|)(?:b)c" (seq [ group (alt [ char 'a'; epsilon ]); char 'b'; char 'c' ]);
  [%expect {||}]
;;

let%expect_test "singleton and longer branches preserve their AST" =
  same_ast "a" (char 'a');
  same_ast "(a)" (group (char 'a'));
  same_ast "(?:ab)" (str "ab");
  same_ast "a*?b+" (seq [ non_greedy (rep (char 'a')); greedy (rep1 (char 'b')) ]);
  same_ast "a(?#x)b" (seq [ char 'a'; char 'b' ]);
  [%expect {||}]
;;

let%expect_test "long sequences remain flat and stack safe" =
  match View.view (Perl.re (String.make 100_000 'a')) with
  | Sequence chars -> assert (List.length chars = 100_000)
  | _ -> assert false
;;
