open Import
open Re

let same_ast pattern expected =
  let show r = Format.asprintf "%a" pp r in
  assert (String.equal (show (Perl.re pattern)) (show expected))
;;

let%expect_test "expressions without alternatives retain their AST" =
  same_ast "" epsilon;
  same_ast "a" (char 'a');
  same_ast "ab" (str "ab");
  same_ast "()" (group epsilon);
  same_ast "(?:ab)" (str "ab");
  same_ast "(?<name>ab)" (group ~name:"name" (str "ab"));
  same_ast "a(?#comment)b" (seq [ char 'a'; char 'b' ]);
  same_ast "a*?b" (seq [ non_greedy (rep (char 'a')); char 'b' ]);
  [%expect {||}]
;;

let%expect_test "alternatives retain source order and empty branches" =
  same_ast "a|b|c" (alt [ char 'a'; char 'b'; char 'c' ]);
  same_ast "|" (alt [ epsilon; epsilon ]);
  same_ast "|a||bc|" (alt [ epsilon; char 'a'; epsilon; str "bc"; epsilon ]);
  same_ast
    "(a|)(?:b|cd)e"
    (seq [ group (alt [ char 'a'; epsilon ]); alt [ char 'b'; str "cd" ]; char 'e' ]);
  same_ast "a\\|b|c" (alt [ str "a|b"; char 'c' ]);
  [%expect {||}]
;;

let%expect_test "errors after the first branch retain their category" =
  List.iter [ "a)"; "a|)"; "a|("; "a|*"; "a|b|["; "(a|b" ] ~f:(fun pattern ->
    match Perl.re_result pattern with
    | Error `Parse_error -> ()
    | _ -> assert false);
  List.iter [ "a|\\1"; "a|b|\\8" ] ~f:(fun pattern ->
    match Perl.re_result pattern with
    | Error `Not_supported -> ()
    | _ -> assert false);
  [%expect {||}]
;;
