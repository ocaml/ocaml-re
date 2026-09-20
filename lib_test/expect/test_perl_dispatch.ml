open Import
open Re

let%expect_test "atom dispatch preserves delimiters and quantifier lookahead" =
  let cases =
    [ "", epsilon
    ; "|", alt [ epsilon; epsilon ]
    ; "(|a)", group (alt [ epsilon; char 'a' ])
    ; "(a|)b", seq [ group (alt [ char 'a'; epsilon ]); char 'b' ]
    ; "a*|b+", alt [ greedy (rep (char 'a')); greedy (rep1 (char 'b')) ]
    ; "a{2,3}?b", seq [ non_greedy (repn (char 'a') 2 (Some 3)); char 'b' ]
    ; "a\\{b", str "a{b"
    ; "a\\|b", str "a|b"
    ; "\\Qab\\E*", greedy (rep (str "ab"))
    ; "(?:a)(b)c", seq [ char 'a'; group (char 'b'); char 'c' ]
    ; "a]}", str "a]}"
    ]
  in
  List.iter cases ~f:(fun (pattern, expected) ->
    let pp r = Format.asprintf "%a" pp r in
    assert (String.equal (pp (Perl.re pattern)) (pp expected)));
  [%expect {||}]
;;

let%expect_test "atom dispatch preserves malformed and unsupported syntax" =
  List.iter
    [ "("
    ; ")"
    ; "a)"
    ; "*"
    ; "+"
    ; "?"
    ; "a**"
    ; "a{"
    ; "a{q"
    ; "a{2"
    ; "a{3,2}"
    ; "(?:a"
    ; "(?"
    ; "a\\"
    ; "[a"
    ; "\\x4"
    ]
    ~f:(fun pattern ->
      match Perl.re_result pattern with
      | Error `Parse_error -> ()
      | _ -> assert false);
  List.iter [ "\\1"; "\\8"; "[\\1]" ] ~f:(fun pattern ->
    match Perl.re_result pattern with
    | Error `Not_supported -> ()
    | _ -> assert false);
  [%expect {||}]
;;
