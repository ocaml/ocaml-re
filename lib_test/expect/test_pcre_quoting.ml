open Import
module Check = Pcre_test_helpers

let%expect_test "quoting individual bytes, including class metacharacters" =
  let checks = Check.create () in
  for code = 0 to 255 do
    let subject = String.make 1 (Char.chr code) in
    List.iter
      [ "\\Q" ^ subject ^ "\\E"; "[\\Q" ^ subject ^ "\\E]" ]
      ~f:(fun pattern ->
        Check.check
          checks
          (Check.text pattern)
          ~expected:(Check.text subject)
          (Check.byte_set ~extra:[ subject ^ subject ] pattern))
  done;
  Check.finish checks;
  [%expect {| 512 checks; 0 differences |}]
;;

let%expect_test "quote delimiters do not create atoms or prevent lazy quantifiers" =
  let checks = Check.create () in
  List.iter
    [ {|\Qab\E+|}, "abbb", "abbb"
    ; {|\Qab\E?|}, "a", "a"
    ; {|\Qab\E{2}|}, "abbb", "abb"
    ; {|a\Q\E+|}, "aaa", "aaa"
    ; {|a+\Q\E?|}, "aaa", "a"
    ; {|a+\E?|}, "aaa", "a"
    ; {|a\E+|}, "aaa", "aaa"
    ; {|\Ea|}, "a", "a"
    ; {|\Qab+|}, "ab+", "ab+"
    ; {|\Qabc\|}, "abc\\", "abc\\"
    ; {|\Q\\E|}, "\\", "\\"
    ; {|\Q\Q\E|}, "\\Q", "\\Q"
    ; {|\E\E\Q\E|}, "", ""
    ; {|(\Qab\E+)(c)|}, "abbbc", "abbbc"
    ; {|\Q(a|b)\E|c|}, "(a|b)", "(a|b)"
    ; {|[a\E-b]|}, "b", "b"
    ; {|[a-\Qb\E]|}, "b", "b"
    ; {|[\Qa\E-b]|}, "b", "b"
    ; {|[a\Q-\Eb]|}, "-", "-"
    ; {|[\Q\E]a]|}, "a", "a"
    ; {|[\Q\E^a]|}, "b", "b"
    ; {|[\Qa-z]\E]|}, "]", "]"
    ]
    ~f:(fun (pattern, subject, expected) ->
      Check.check
        checks
        (Printf.sprintf "%S on %S" pattern subject)
        ~expected:(Check.text expected)
        (Check.match_text pattern subject));
  Check.finish checks;
  [%expect {| 22 checks; 0 differences |}]
;;

let%expect_test "quoted quantifiers, ranges and capture syntax are literal" =
  let checks = Check.create () in
  List.iter
    [ {|\Qa+\E|}, "aaa"; {|[\Qa-z\E]|}, "b"; {|\Q(a)\E|}, "a"; {|\Qab\E+|}, "abab" ]
    ~f:(fun (pattern, subject) ->
      Check.check
        checks
        (Printf.sprintf "%S on %S" pattern subject)
        ~expected:"no match"
        (Check.match_text ~whole:true pattern subject));
  let pattern = {|(\Qab\E+)(c)|} in
  Check.check
    checks
    (Check.text pattern)
    ~expected:{|["abbbc"; "abbb"; "c"]|}
    (Check.with_match pattern "abbbc" (fun _ groups ->
       Check.array Check.text (Re.Group.all groups)));
  Check.finish checks;
  [%expect {| 5 checks; 0 differences |}]
;;

let%expect_test "quotes do not repair invalid syntax or unterminated classes" =
  let checks = Check.create () in
  List.iter
    [ {|\Q\E+|}
    ; {|\E*|}
    ; {|[\Qabc]|}
    ; {|[\Q\E]|}
    ; {|(\Qabc)|}
    ; {|(?<a\Q\E>b)|}
    ; {|\x{\Q41\E}|}
    ]
    ~f:(fun pattern ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:"parse error"
        (Check.parse_status pattern));
  Check.finish checks;
  [%expect {| 7 checks; 0 differences |}]
;;
