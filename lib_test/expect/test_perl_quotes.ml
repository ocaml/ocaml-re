open Import
open Re

let same_ast pattern expected =
  let show r = Format.asprintf "%a" pp r in
  assert (String.equal (show (Perl.re pattern)) (show expected))
;;

let parse_error pattern =
  match Perl.re_result pattern with
  | Error `Parse_error -> ()
  | Ok _ | Error `Not_supported -> assert false
;;

let%expect_test "quoted text preserves escape pairs and atom boundaries" =
  same_ast {|\Q\E|} epsilon;
  same_ast {|\Qab\E|} (str "ab");
  same_ast {|a\Qbc\E*|} (seq [ char 'a'; char 'b'; greedy (rep (char 'c')) ]);
  same_ast {|\Q\\E\E|} (char '\\');
  same_ast {|\Q\q.[]*\E|} (str {|\q.[]*|});
  same_ast {|\Q(\E|} (char '(');
  for i = 0 to 255 do
    let text = String.make 1 (Char.chr i) ^ "x" in
    same_ast ("\\Q" ^ text ^ "\\E") (str text)
  done;
  [%expect {||}]
;;

let%expect_test "a quantifier applies to the last quoted byte" =
  same_ast {|\Qab\E?|} (seq [ char 'a'; greedy (opt (char 'b')) ]);
  same_ast {|\Qab\E{2}|} (seq [ char 'a'; greedy (repn (char 'b') 2 (Some 2)) ]);
  same_ast {|\Qab\E+?|} (seq [ char 'a'; non_greedy (rep1 (char 'b')) ]);
  [%expect {||}]
;;

let%expect_test "unterminated quotes extend to the end of the pattern" =
  same_ast {|\Q|} epsilon;
  same_ast {|\Qabc|} (str "abc");
  same_ast {|\Qabc\|} (str {|abc\|});
  same_ast {|\Q\\E|} (char '\\');
  same_ast {|\Q\|} (char '\\');
  [%expect {||}]
;;

let%expect_test "an empty quote does not satisfy a quantifier" =
  List.iter ~f:parse_error [ {|\Q\E+|}; {|\Q\E*|} ];
  [%expect {||}]
;;

let%expect_test "long quoted text is stack safe" =
  let text = String.make 100_000 'a' in
  (match View.view (Perl.re ("\\Q" ^ text ^ "\\E")) with
   | Sequence chars -> assert (List.length chars = String.length text)
   | _ -> assert false);
  [%expect {||}]
;;
