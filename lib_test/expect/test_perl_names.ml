open Import
open Re

let named name body = "(?<" ^ name ^ ">" ^ body ^ ")"

let same_ast pattern expected =
  let show r = Format.asprintf "%a" pp r in
  assert (String.equal (show (Perl.re pattern)) (show expected))
;;

let parse_error pattern =
  match Perl.re_result pattern with
  | Error `Parse_error -> ()
  | Ok _ | Error `Not_supported -> assert false
;;

let%expect_test "capture names preserve ASTs and capture indices" =
  same_ast (named "a_09" "ab") (group ~name:"a_09" (str "ab"));
  same_ast (named "_" "") (group ~name:"_" epsilon);
  same_ast "(?<a>>b)" (group ~name:"a" (str ">b"));
  let pattern = "x(?<_A1>(a))(?<x9>b)y" in
  same_ast
    pattern
    (seq
       [ char 'x'
       ; group ~name:"_A1" (group (char 'a'))
       ; group ~name:"x9" (char 'b')
       ; char 'y'
       ]);
  let re = Perl.compile_pat pattern in
  let names = group_names re in
  assert (List.assoc "_A1" names = 1);
  assert (List.assoc "x9" names = 3);
  let matched = exec re "xaby" in
  assert (String.equal (Group.get matched 1) "a");
  assert (String.equal (Group.get matched 2) "a");
  assert (String.equal (Group.get matched 3) "b");
  [%expect {||}]
;;

let%expect_test "capture name character rules cover every byte" =
  for byte = 0 to 255 do
    let c = Char.chr byte in
    let s = String.make 1 c in
    let initial =
      match c with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false
    in
    if initial
    then same_ast (named s "a") (group ~name:s (char 'a'))
    else parse_error (named s "a");
    (* A closing angle bracket ends the name rather than belonging to it. *)
    if not (Char.equal c '>')
    then (
      let name = "a" ^ s in
      match c with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
        same_ast (named name "a") (group ~name (char 'a'))
      | _ -> parse_error (named name "a"))
  done;
  [%expect {||}]
;;

let%expect_test "incomplete and invalid capture names remain parse errors" =
  List.iter
    ~f:parse_error
    [ "(?<"; "(?<>a)"; "(?<0>a)"; "(?<a"; "(?<a>"; "(?<a>b"; "(?<a-b>x)" ];
  [%expect {||}]
;;

let%expect_test "long capture names remain intact and stack safe" =
  let name = String.make 100_000 'a' in
  (match View.view (Perl.re (named name "a")) with
   | Group (Some actual, _) -> assert (String.equal actual name)
   | _ -> assert false);
  parse_error ("(?<" ^ name);
  [%expect {||}]
;;
