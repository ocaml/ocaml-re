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
  [%expect
    {|
    "[\\Q\000\\E]": parse error; expected "\000"
    "[\\Q\001\\E]": parse error; expected "\001"
    "[\\Q\002\\E]": parse error; expected "\002"
    "[\\Q\003\\E]": parse error; expected "\003"
    "[\\Q\004\\E]": parse error; expected "\004"
    "[\\Q\005\\E]": parse error; expected "\005"
    ... 251 more differences
    512 checks; 257 differences
    |}]
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
  [%expect
    {|
    "\\Qab\\E+" on "abbb": "ab"; expected "abbb"
    "\\Qab\\E?" on "a": ""; expected "a"
    "\\Qab\\E{2}" on "abbb": no match; expected "abb"
    "a\\Q\\E+" on "aaa": "a"; expected "aaa"
    "a+\\Q\\E?" on "aaa": "aaa"; expected "a"
    "a+\\E?" on "aaa": parse error; expected "a"
    ... 14 more differences
    22 checks; 20 differences
    |}]
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
  [%expect
    {|
    "[\\Qa-z\\E]" on "b": parse error; expected no match
    "\\Qab\\E+" on "abab": "abab"; expected no match
    "(\\Qab\\E+)(c)": no match; expected ["abbbc"; "abbb"; "c"]
    5 checks; 3 differences
    |}]
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
  [%expect
    {|
    "\\Q\\E+": compiled; expected parse error
    7 checks; 1 differences
    |}]
;;
