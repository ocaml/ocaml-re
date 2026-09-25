open Import
module Check = Pcre_test_helpers

type outcome =
  | Match of string
  | No_match
  | Parse_error
  | Not_supported

let show = function
  | Match s -> Printf.sprintf "match %S" s
  | No_match -> "no match"
  | Parse_error -> "parse error"
  | Not_supported -> "not supported"
;;

let run parse pattern subject =
  match parse pattern with
  | Error `Parse_error -> Parse_error
  | Error `Not_supported -> Not_supported
  | Ok re ->
    (match Re.exec_opt (Re.compile re) subject with
     | None -> No_match
     | Some groups -> Match (Re.Group.get groups 0))
;;

let check ?(opts = []) cases =
  let checks = Check.create ~max_examples:(List.length cases) () in
  List.iter cases ~f:(fun (pattern, subject, expected) ->
    let actual = run (Re.Perl.re_result ~opts) pattern subject in
    if Poly.equal opts []
    then assert (Poly.equal actual (run Re.Pcre.re_result pattern subject));
    Check.check
      checks
      (Printf.sprintf "%S on %S" pattern subject)
      ~expected:(show expected)
      (show actual));
  Check.finish checks
;;

let%expect_test "character tables and atomic newline sequences" =
  check
    [ {|\w|}, "\233", No_match
    ; {|[[:alpha:]]|}, "\233", No_match
    ; {|\R|}, "\r\n", Match "\r\n"
    ; {|\R\n|}, "\r\n", No_match
    ; {|\R|}, "\133", Match "\133"
    ];
  [%expect
    {|
    "\\w" on "\233": match "\233"; expected no match
    "[[:alpha:]]" on "\233": match "\233"; expected no match
    "\\R" on "\r\n": parse error; expected match "\r\n"
    "\\R\\n" on "\r\n": parse error; expected no match
    "\\R" on "\133": parse error; expected match "\133"
    5 checks; 5 differences
    |}]
;;

let%expect_test "quantifier syntax and literal braces" =
  check
    [ {|a{,2}|}, "aaa", Match "aa"
    ; {|a{ 1 , 2 }|}, "aaa", Match "aa"
    ; {|a{foo}|}, "a{foo}", Match "a{foo}"
    ; {|a{1x}|}, "a{1x}", Match "a{1x}"
    ; {|a{,}|}, "a{,}", Match "a{,}"
    ; {|{foo}|}, "{foo}", Match "{foo}"
    ; {|a{\Q1\E,2}|}, "a{1,2}", Match "a{1,2}"
    ];
  [%expect
    {|
    "a{,2}" on "aaa": parse error; expected match "aa"
    "a{ 1 , 2 }" on "aaa": parse error; expected match "aa"
    "a{foo}" on "a{foo}": parse error; expected match "a{foo}"
    "a{1x}" on "a{1x}": parse error; expected match "a{1x}"
    "a{,}" on "a{,}": parse error; expected match "a{,}"
    "{foo}" on "{foo}": parse error; expected match "{foo}"
    "a{\\Q1\\E,2}" on "a{1,2}": parse error; expected match "a{1,2}"
    7 checks; 7 differences
    |}]
;;

let%expect_test "match reset and final-newline start anchor" =
  check [ {|foo\Kbar|}, "foobar", Match "bar" ];
  check ~opts:[ `Multiline ] [ {|\n^|}, "a\n", No_match ];
  [%expect
    {|
    "foo\\Kbar" on "foobar": parse error; expected match "bar"
    1 checks; 1 differences
    "\\n^" on "a\n": match "\n"; expected no match
    1 checks; 1 differences
    |}]
;;

let%expect_test "branch reset and internal options" =
  check
    [ {|(?|(a)|(b))|}, "b", Match "b"
    ; {|(?i)a|}, "A", Match "A"
    ; {|(?i:a)b|}, "Ab", Match "Ab"
    ; {|(?i)a(?-i)b|}, "Ab", Match "Ab"
    ; {|(?m)^a|}, "x\na", Match "a"
    ; {|(?s).|}, "\n", Match "\n"
    ; {|(?U)a+|}, "aaa", Match "a"
    ; {|(?n)(a)|}, "a", Match "a"
    ; ( {|(?x) a # comment
 b|}
      , "ab"
      , Match "ab" )
    ; {|(?xx)[ a ]|}, "a", Match "a"
    ; {|(?J)(?<a>x)|(?<a>y)|}, "y", Match "y"
    ; {|(?)a|}, "a", Match "a"
    ; {|(?^)a|}, "a", Match "a"
    ; {|(?aT)[[:digit:]]|}, "1", Match "1"
    ];
  [%expect
    {|
    "(?|(a)|(b))" on "b": parse error; expected match "b"
    "(?i)a" on "A": parse error; expected match "A"
    "(?i:a)b" on "Ab": parse error; expected match "Ab"
    "(?i)a(?-i)b" on "Ab": parse error; expected match "Ab"
    "(?m)^a" on "x\na": parse error; expected match "a"
    "(?s)." on "\n": parse error; expected match "\n"
    "(?U)a+" on "aaa": parse error; expected match "a"
    "(?n)(a)" on "a": parse error; expected match "a"
    "(?x) a # comment\n b" on "ab": parse error; expected match "ab"
    "(?xx)[ a ]" on "a": parse error; expected match "a"
    "(?J)(?<a>x)|(?<a>y)" on "y": parse error; expected match "y"
    "(?)a" on "a": parse error; expected match "a"
    "(?^)a" on "a": parse error; expected match "a"
    "(?aT)[[:digit:]]" on "1": parse error; expected match "1"
    14 checks; 14 differences
    |}]
;;

let%expect_test "extended character classes" =
  check
    [ {|(?[ [a-z] & [^aeiou] ])|}, "b", Match "b"
    ; {|(?[ [a-z] - [aeiou] ])|}, "b", Match "b"
    ; {|(?[ [a] | [b] ])|}, "b", Match "b"
    ; {|(?[ [a] + [b] ])|}, "b", Match "b"
    ; {|(?[ [ab] ^ [bc] ])|}, "a", Match "a"
    ; {|(?[ ![a] ])|}, "b", Match "b"
    ];
  [%expect
    {|
    "(?[ [a-z] & [^aeiou] ])" on "b": parse error; expected match "b"
    "(?[ [a-z] - [aeiou] ])" on "b": parse error; expected match "b"
    "(?[ [a] | [b] ])" on "b": parse error; expected match "b"
    "(?[ [a] + [b] ])" on "b": parse error; expected match "b"
    "(?[ [ab] ^ [bc] ])" on "a": parse error; expected match "a"
    "(?[ ![a] ])" on "b": parse error; expected match "b"
    6 checks; 6 differences
    |}]
;;

let%expect_test "lookaround, atomic groups and possessive quantifiers" =
  check
    [ {|a(?=b)|}, "ab", Match "a"
    ; {|a(?!b)|}, "ac", Match "a"
    ; {|(?<=a)b|}, "ab", Match "b"
    ; {|(?<!a)b|}, "cb", Match "b"
    ; {|(?<=ab?)c|}, "abc", Match "c"
    ; {|(*pla:a)a|}, "a", Match "a"
    ; {|(*negative_lookahead:b)a|}, "a", Match "a"
    ; {|(*plb:a)b|}, "ab", Match "b"
    ; {|(*negative_lookbehind:a)b|}, "cb", Match "b"
    ; {|(?*a)a|}, "a", Match "a"
    ; {|(?<*a)b|}, "ab", Match "b"
    ; {|(*napla:a)a|}, "a", Match "a"
    ; {|(*naplb:a)b|}, "ab", Match "b"
    ; {|(?>a|ab)c|}, "abc", No_match
    ; {|(*atomic:a|ab)c|}, "abc", No_match
    ; {|a*+a|}, "aaa", No_match
    ; {|a++a|}, "aaa", No_match
    ; {|a?+a|}, "a", No_match
    ; {|a{1,2}+a|}, "aa", No_match
    ; {|(?!)|}, "", No_match
    ; {|(?=)|}, "", Match ""
    ];
  [%expect
    {|
    "a(?=b)" on "ab": parse error; expected match "a"
    "a(?!b)" on "ac": parse error; expected match "a"
    "(?<=a)b" on "ab": parse error; expected match "b"
    "(?<!a)b" on "cb": parse error; expected match "b"
    "(?<=ab?)c" on "abc": parse error; expected match "c"
    "(*pla:a)a" on "a": parse error; expected match "a"
    "(*negative_lookahead:b)a" on "a": parse error; expected match "a"
    "(*plb:a)b" on "ab": parse error; expected match "b"
    "(*negative_lookbehind:a)b" on "cb": parse error; expected match "b"
    "(?*a)a" on "a": parse error; expected match "a"
    "(?<*a)b" on "ab": parse error; expected match "b"
    "(*napla:a)a" on "a": parse error; expected match "a"
    "(*naplb:a)b" on "ab": parse error; expected match "b"
    "(?>a|ab)c" on "abc": parse error; expected no match
    "(*atomic:a|ab)c" on "abc": parse error; expected no match
    "a*+a" on "aaa": parse error; expected no match
    "a++a" on "aaa": parse error; expected no match
    "a?+a" on "a": parse error; expected no match
    "a{1,2}+a" on "aa": parse error; expected no match
    "(?!)" on "": parse error; expected no match
    "(?=)" on "": parse error; expected match ""
    21 checks; 21 differences
    |}]
;;

let%expect_test "conditions, subroutines and recursion" =
  check
    [ {|(a)?(?(1)b|c)|}, "ab", Match "ab"
    ; {|(?<a>a)?(?(<a>)b|c)|}, "c", Match "c"
    ; {|(a)?(?(-1)b|c)|}, "ab", Match "ab"
    ; {|(?(?=a)a|b)|}, "a", Match "a"
    ; {|(?(VERSION>=10.0)a|b)|}, "a", Match "a"
    ; {|(?(R)b|a)|}, "a", Match "a"
    ; {|(?(DEFINE)(?<a>a))(?&a)|}, "a", Match "a"
    ; {|(a)(?1)|}, "aa", Match "aa"
    ; {|(a)(?-1)|}, "aa", Match "aa"
    ; {|(?+1)(a)|}, "aa", Match "aa"
    ; {|(?<a>a)(?P>a)|}, "aa", Match "aa"
    ; {|(a)\g<1>|}, "aa", Match "aa"
    ; {|(?<a>a)\g'a'|}, "aa", Match "aa"
    ; {|a(?R)?b|}, "aabb", Match "aabb"
    ; {|a(?0)?b|}, "aabb", Match "aabb"
    ; {|(?<a>a)(?&a(<a>))|}, "aa", Match "aa"
    ; {|(a)(*scan_substring:(1)a)|}, "a", Match "a"
    ; {|(a)(*scs:(1)a)|}, "a", Match "a"
    ];
  [%expect
    {|
    "(a)?(?(1)b|c)" on "ab": parse error; expected match "ab"
    "(?<a>a)?(?(<a>)b|c)" on "c": parse error; expected match "c"
    "(a)?(?(-1)b|c)" on "ab": parse error; expected match "ab"
    "(?(?=a)a|b)" on "a": parse error; expected match "a"
    "(?(VERSION>=10.0)a|b)" on "a": parse error; expected match "a"
    "(?(R)b|a)" on "a": parse error; expected match "a"
    "(?(DEFINE)(?<a>a))(?&a)" on "a": parse error; expected match "a"
    "(a)(?1)" on "aa": parse error; expected match "aa"
    "(a)(?-1)" on "aa": parse error; expected match "aa"
    "(?+1)(a)" on "aa": parse error; expected match "aa"
    "(?<a>a)(?P>a)" on "aa": parse error; expected match "aa"
    "(a)\\g<1>" on "aa": parse error; expected match "aa"
    "(?<a>a)\\g'a'" on "aa": parse error; expected match "aa"
    "a(?R)?b" on "aabb": parse error; expected match "aabb"
    "a(?0)?b" on "aabb": parse error; expected match "aabb"
    "(?<a>a)(?&a(<a>))" on "aa": parse error; expected match "aa"
    "(a)(*scan_substring:(1)a)" on "a": parse error; expected match "a"
    "(a)(*scs:(1)a)" on "a": parse error; expected match "a"
    18 checks; 18 differences
    |}]
;;

let%expect_test "verbs, callouts, and start-of-pattern directives" =
  check
    [ {|a(*FAIL)|b|}, "ab", Match "b"
    ; {|a(*F)|b|}, "ab", Match "b"
    ; {|a(*ACCEPT)b|}, "a", Match "a"
    ; {|a(*MARK:here)|}, "a", Match "a"
    ; {|a(*:here)|}, "a", Match "a"
    ; {|a(*COMMIT)b|ac|}, "ac", No_match
    ; {|a(*PRUNE)b|ac|}, "ac", No_match
    ; {|a(*SKIP)(*F)|.|}, "ab", Match "b"
    ; {|a(*THEN)b|ac|}, "ac", Match "ac"
    ; {|a(?C)b|}, "ab", Match "ab"
    ; {|a(?C1)b|}, "ab", Match "ab"
    ; {|a(?C"note")b|}, "ab", Match "ab"
    ; {|(*NOTEMPTY)a*|}, "b", No_match
    ; {|(*NOTEMPTY_ATSTART)a*|}, "b", Match ""
    ; {|(*NO_AUTO_POSSESS)a|}, "a", Match "a"
    ; {|(*NO_DOTSTAR_ANCHOR)a|}, "a", Match "a"
    ; {|(*NO_JIT)a|}, "a", Match "a"
    ; {|(*NO_START_OPT)a|}, "a", Match "a"
    ; {|(*LIMIT_DEPTH=100)a|}, "a", Match "a"
    ; {|(*LIMIT_RECURSION=100)a|}, "a", Match "a"
    ; {|(*LIMIT_HEAP=100)a|}, "a", Match "a"
    ; {|(*LIMIT_MATCH=100)a|}, "a", Match "a"
    ; {|(*CR).|}, "\r", No_match
    ; {|(*LF).|}, "\n", No_match
    ; {|(*CRLF).|}, "\n", Match "\n"
    ; {|(*ANYCRLF).|}, "\r", No_match
    ; {|(*ANY).|}, "\133", No_match
    ; {|(*NUL).|}, "\000", No_match
    ; {|(*BSR_ANYCRLF)\R|}, "\133", No_match
    ; {|(*BSR_UNICODE)\R|}, "\133", Match "\133"
    ];
  [%expect
    {|
    "a(*FAIL)|b" on "ab": parse error; expected match "b"
    "a(*F)|b" on "ab": parse error; expected match "b"
    "a(*ACCEPT)b" on "a": parse error; expected match "a"
    "a(*MARK:here)" on "a": parse error; expected match "a"
    "a(*:here)" on "a": parse error; expected match "a"
    "a(*COMMIT)b|ac" on "ac": parse error; expected no match
    "a(*PRUNE)b|ac" on "ac": parse error; expected no match
    "a(*SKIP)(*F)|." on "ab": parse error; expected match "b"
    "a(*THEN)b|ac" on "ac": parse error; expected match "ac"
    "a(?C)b" on "ab": parse error; expected match "ab"
    "a(?C1)b" on "ab": parse error; expected match "ab"
    "a(?C\"note\")b" on "ab": parse error; expected match "ab"
    "(*NOTEMPTY)a*" on "b": parse error; expected no match
    "(*NOTEMPTY_ATSTART)a*" on "b": parse error; expected match ""
    "(*NO_AUTO_POSSESS)a" on "a": parse error; expected match "a"
    "(*NO_DOTSTAR_ANCHOR)a" on "a": parse error; expected match "a"
    "(*NO_JIT)a" on "a": parse error; expected match "a"
    "(*NO_START_OPT)a" on "a": parse error; expected match "a"
    "(*LIMIT_DEPTH=100)a" on "a": parse error; expected match "a"
    "(*LIMIT_RECURSION=100)a" on "a": parse error; expected match "a"
    "(*LIMIT_HEAP=100)a" on "a": parse error; expected match "a"
    "(*LIMIT_MATCH=100)a" on "a": parse error; expected match "a"
    "(*CR)." on "\r": parse error; expected no match
    "(*LF)." on "\n": parse error; expected no match
    "(*CRLF)." on "\n": parse error; expected match "\n"
    "(*ANYCRLF)." on "\r": parse error; expected no match
    "(*ANY)." on "\133": parse error; expected no match
    "(*NUL)." on "\000": parse error; expected no match
    "(*BSR_ANYCRLF)\\R" on "\133": parse error; expected no match
    "(*BSR_UNICODE)\\R" on "\133": parse error; expected match "\133"
    30 checks; 30 differences
    |}]
;;

let%expect_test "invalid classes, quantified assertions and duplicate names" =
  check
    [ {|[z-a]|}, "z", Parse_error
    ; {|[a-\d]|}, "a", Parse_error
    ; {|[\d-a]|}, "a", Parse_error
    ; {|[[.a.]]|}, "a", Parse_error
    ; {|[[=a=]]|}, "a", Parse_error
    ; {|^*|}, "", Parse_error
    ; {|(?<a>x)(?<a>y)|}, "xy", Parse_error
    ];
  [%expect
    {|
    "[z-a]" on "z": match "z"; expected parse error
    "[a-\\d]" on "a": match "a"; expected parse error
    "[\\d-a]" on "a": match "a"; expected parse error
    "[[.a.]]" on "a": match "a"; expected parse error
    "[[=a=]]" on "a": not supported; expected parse error
    "^*" on "": match ""; expected parse error
    "(?<a>x)(?<a>y)" on "xy": match "xy"; expected parse error
    7 checks; 7 differences
    |}]
;;

let%expect_test "capture bookkeeping for zero repetitions" =
  let re = Re.Pcre.regexp "(a){0}(b)" in
  let groups = Re.exec re "b" in
  Printf.printf "group count: %d (PCRE: 3)\n" (Re.group_count re);
  Printf.printf
    "group 1: %s (PCRE: unset)\n"
    (match Re.Group.get_opt groups 1 with
     | None -> "unset"
     | Some s -> Printf.sprintf "%S" s);
  [%expect
    {|
    group count: 3 (PCRE: 3)
    group 1: unset (PCRE: unset)
    |}]
;;
