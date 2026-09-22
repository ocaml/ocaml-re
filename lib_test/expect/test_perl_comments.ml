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

let%expect_test "comments leave no trace in the AST" =
  same_ast "(?#)" epsilon;
  same_ast "a(?#text)b" (seq [ char 'a'; char 'b' ]);
  same_ast "(?#one)(?#two)" epsilon;
  same_ast "(a(?#inner))(b)" (seq [ group (char 'a'); group (char 'b') ]);
  same_ast "a(?#text)*?b" (seq [ non_greedy (rep (char 'a')); char 'b' ]);
  [%expect {||}]
;;

let%expect_test "comments stop at the first closing parenthesis" =
  same_ast "(?#a\\)b" (char 'b');
  same_ast "(?#(nested)b" (char 'b');
  for byte = 0 to 255 do
    let c = Char.chr byte in
    let pattern = "(?#" ^ String.make 1 c ^ ")" in
    if Char.equal c ')' then parse_error pattern else same_ast pattern epsilon
  done;
  [%expect {||}]
;;

let%expect_test "unterminated comments remain parse errors" =
  List.iter ~f:parse_error [ "(?#"; "a(?#text"; "(?#\\"; "(?#(unfinished"; "((?#text)" ];
  [%expect {||}]
;;

let%expect_test "long comments remain stack safe" =
  let text = String.make 100_000 'a' in
  same_ast ("a(?#" ^ text ^ ")b") (seq [ char 'a'; char 'b' ]);
  parse_error ("(?#" ^ text);
  [%expect {||}]
;;
