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

let%expect_test "comments retain epsilon nodes and quantifier binding" =
  same_ast "(?#)" epsilon;
  same_ast "a(?#text)b" (seq [ char 'a'; epsilon; char 'b' ]);
  same_ast "(?#one)(?#two)" (seq [ epsilon; epsilon ]);
  same_ast "(a(?#inner))(b)" (seq [ group (seq [ char 'a'; epsilon ]); group (char 'b') ]);
  same_ast "a(?#text)*?b" (seq [ char 'a'; non_greedy (rep epsilon); char 'b' ]);
  [%expect {||}]
;;

let%expect_test "comments stop at the first closing parenthesis" =
  same_ast "(?#a\\)b" (seq [ epsilon; char 'b' ]);
  same_ast "(?#(nested)b" (seq [ epsilon; char 'b' ]);
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
  same_ast ("a(?#" ^ text ^ ")b") (seq [ char 'a'; epsilon; char 'b' ]);
  parse_error ("(?#" ^ text);
  [%expect {||}]
;;
