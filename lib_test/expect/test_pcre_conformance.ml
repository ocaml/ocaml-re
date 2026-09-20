open Import

(* Byte-mode PCRE2 pattern audit. See PCRE_SUPPORT.md for the scope and sources.
   These are characterization tests: print only disagreements with PCRE2, so a
   missing feature and a silently misinterpreted feature are both visible.
   Keep the PCRE expectations when implementing a feature; update the snapshot.
   Unicode and backreferences are deliberately not part of this suite. *)
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
  let conforming = ref 0 in
  List.iter cases ~f:(fun (pattern, subject, expected) ->
    let actual = run (Re.Perl.re_result ~opts) pattern subject in
    if Poly.equal opts []
    then assert (Poly.equal actual (run Re.Pcre.re_result pattern subject));
    if Poly.equal actual expected
    then incr conforming
    else
      Printf.printf
        "%S on %S: %s; PCRE expects %s\n"
        pattern
        subject
        (show actual)
        (show expected));
  Printf.printf "%d/%d conforming\n" !conforming (List.length cases)
;;

let%expect_test "byte escapes, including class and range contexts" =
  check
    [ {|\a|}, "\007", Match "\007"
    ; {|\cA|}, "\001", Match "\001"
    ; {|\cz|}, "\026", Match "\026"
    ; {|\c?|}, "\127", Match "\127"
    ; {|\0|}, "\000", Match "\000"
    ; {|\07x|}, "\007x", Match "\007x"
    ; {|\08|}, "\0008", Match "\0008"
    ; {|\11x|}, "\tx", Match "\tx"
    ; {|\xA!|}, "\n!", Match "\n!"
    ; {|\x{41}|}, "A", Match "A"
    ; {|\x{000041}|}, "A", Match "A"
    ; {|\x{ 41 }|}, "A", Match "A"
    ; {|\o{101}|}, "A", Match "A"
    ; {|\o{000101}|}, "A", Match "A"
    ; {|\o{ 101 }|}, "A", Match "A"
    ; {|[\a\e\f]|}, "\007", Match "\007"
    ; {|[\cA-\cZ]|}, "\026", Match "\026"
    ; {|[\x41-\x5a]|}, "Z", Match "Z"
    ; {|[\o{101}]|}, "A", Match "A"
    ; {|[\0]|}, "\000", Match "\000"
    ; {|[\7]|}, "\007", Match "\007"
    ; {|[\11x]|}, "x", Match "x"
    ; {|[\101-\132]|}, "Z", Match "Z"
    ; {|[\8\9]|}, "9", Match "9"
    ];
  [%expect
    {|
    "\\a" on "\007": parse error; PCRE expects match "\007"
    "\\cA" on "\001": parse error; PCRE expects match "\001"
    "\\cz" on "\026": parse error; PCRE expects match "\026"
    "\\c?" on "\127": parse error; PCRE expects match "\127"
    "\\0" on "\000": not supported; PCRE expects match "\000"
    "\\07x" on "\007x": not supported; PCRE expects match "\007x"
    "\\08" on "\0008": not supported; PCRE expects match "\0008"
    "\\11x" on "\tx": not supported; PCRE expects match "\tx"
    "\\xA!" on "\n!": parse error; PCRE expects match "\n!"
    "\\x{000041}" on "A": parse error; PCRE expects match "A"
    "\\x{ 41 }" on "A": parse error; PCRE expects match "A"
    "\\o{ 101 }" on "A": parse error; PCRE expects match "A"
    "[\\a\\e\\f]" on "\007": parse error; PCRE expects match "\007"
    "[\\cA-\\cZ]" on "\026": parse error; PCRE expects match "\026"
    "[\\x41-\\x5a]" on "Z": parse error; PCRE expects match "Z"
    "[\\o{101}]" on "A": parse error; PCRE expects match "A"
    "[\\0]" on "\000": not supported; PCRE expects match "\000"
    "[\\7]" on "\007": not supported; PCRE expects match "\007"
    "[\\11x]" on "x": not supported; PCRE expects match "x"
    "[\\101-\\132]" on "Z": not supported; PCRE expects match "Z"
    "[\\8\\9]" on "9": not supported; PCRE expects match "9"
    3/24 conforming
    |}]
;;

let%expect_test "byte character types" =
  check
    [ {|\h|}, "\t", Match "\t"
    ; {|\h|}, "\160", Match "\160"
    ; {|\H|}, "a", Match "a"
    ; {|\v|}, "\133", Match "\133"
    ; {|\V|}, "a", Match "a"
    ; {|[\h\v]|}, "\r", Match "\r"
    ; {|[\H]|}, "a", Match "a"
    ; {|[\V]|}, "a", Match "a"
    ; {|\N|}, "a", Match "a"
    ; {|\N|}, "\n", No_match
    ; {|\C|}, "\n", Match "\n"
    ; {|[\W]|}, "!", Match "!"
    ; {|[\W]|}, "a", No_match
    ; {|[^\W_]|}, "a", Match "a"
    ; {|\w|}, "\233", No_match
    ; {|[[:alpha:]]|}, "\233", No_match
    ; {|\R|}, "\r\n", Match "\r\n"
    ; {|\R\n|}, "\r\n", No_match
    ; {|\R|}, "\133", Match "\133"
    ];
  [%expect
    {|
    "\\h" on "\t": parse error; PCRE expects match "\t"
    "\\h" on "\160": parse error; PCRE expects match "\160"
    "\\H" on "a": parse error; PCRE expects match "a"
    "\\v" on "\133": parse error; PCRE expects match "\133"
    "\\V" on "a": parse error; PCRE expects match "a"
    "[\\h\\v]" on "\r": parse error; PCRE expects match "\r"
    "[\\H]" on "a": parse error; PCRE expects match "a"
    "[\\V]" on "a": parse error; PCRE expects match "a"
    "\\N" on "a": parse error; PCRE expects match "a"
    "\\N" on "\n": parse error; PCRE expects no match
    "\\C" on "\n": parse error; PCRE expects match "\n"
    "[\\W]" on "!": no match; PCRE expects match "!"
    "[\\W]" on "a": match "a"; PCRE expects no match
    "[^\\W_]" on "a": no match; PCRE expects match "a"
    "\\w" on "\233": match "\233"; PCRE expects no match
    "[[:alpha:]]" on "\233": match "\233"; PCRE expects no match
    "\\R" on "\r\n": parse error; PCRE expects match "\r\n"
    "\\R\\n" on "\r\n": parse error; PCRE expects no match
    "\\R" on "\133": parse error; PCRE expects match "\133"
    0/19 conforming
    |}]
;;

let%expect_test "quoting and quantifiers" =
  check
    [ {|\Qabc|}, "abc", Match "abc"
    ; {|\Qabc\|}, "abc\\", Match "abc\\"
    ; {|\Qabc\E+|}, "abccc", Match "abccc"
    ; {|\Q\E+|}, "", Parse_error
    ; {|a\Q\E+|}, "aaa", Match "aaa"
    ; {|\Ea|}, "a", Match "a"
    ; {|\Q\\E|}, "\\", Match "\\"
    ; {|[\Qa-z]\E]|}, "-", Match "-"
    ; {|[\Q]\E]|}, "]", Match "]"
    ; {|a{,2}|}, "aaa", Match "aa"
    ; {|a{ 1 , 2 }|}, "aaa", Match "aa"
    ; {|a{foo}|}, "a{foo}", Match "a{foo}"
    ; {|a{1x}|}, "a{1x}", Match "a{1x}"
    ; {|a{,}|}, "a{,}", Match "a{,}"
    ; {|{foo}|}, "{foo}", Match "{foo}"
    ; {|a{\Q1\E,2}|}, "a{1,2}", Match "a{1,2}"
    ];
  [%expect
    {|
    "\\Qabc" on "abc": parse error; PCRE expects match "abc"
    "\\Qabc\\" on "abc\\": parse error; PCRE expects match "abc\\"
    "\\Qabc\\E+" on "abccc": match "abc"; PCRE expects match "abccc"
    "\\Q\\E+" on "": match ""; PCRE expects parse error
    "a\\Q\\E+" on "aaa": match "a"; PCRE expects match "aaa"
    "\\Ea" on "a": parse error; PCRE expects match "a"
    "\\Q\\\\E" on "\\": parse error; PCRE expects match "\\"
    "[\\Qa-z]\\E]" on "-": parse error; PCRE expects match "-"
    "[\\Q]\\E]" on "]": parse error; PCRE expects match "]"
    "a{,2}" on "aaa": parse error; PCRE expects match "aa"
    "a{ 1 , 2 }" on "aaa": parse error; PCRE expects match "aa"
    "a{foo}" on "a{foo}": parse error; PCRE expects match "a{foo}"
    "a{1x}" on "a{1x}": parse error; PCRE expects match "a{1x}"
    "a{,}" on "a{,}": parse error; PCRE expects match "a{,}"
    "{foo}" on "{foo}": parse error; PCRE expects match "{foo}"
    "a{\\Q1\\E,2}" on "a{1,2}": parse error; PCRE expects match "a{1,2}"
    0/16 conforming
    |}]
;;

let%expect_test "anchors and word boundary aliases" =
  check
    [ {|a$|}, "a\n", Match "a"
    ; {|a$|}, "a\nb", No_match
    ; {|[[:<:]]a|}, "a", Match "a"
    ; {|a[[:>:]]|}, "a", Match "a"
    ; {|foo\Kbar|}, "foobar", Match "bar"
    ];
  check ~opts:[ `Dollar_endonly ] [ {|a$|}, "a\n", No_match ];
  check ~opts:[ `Multiline ] [ {|\n^|}, "a\n", No_match ];
  [%expect
    {|
    "a$" on "a\n": no match; PCRE expects match "a"
    "[[:<:]]a" on "a": parse error; PCRE expects match "a"
    "a[[:>:]]" on "a": parse error; PCRE expects match "a"
    "foo\\Kbar" on "foobar": parse error; PCRE expects match "bar"
    1/5 conforming
    "a$" on "a\n": match "a"; PCRE expects no match
    0/1 conforming
    "\\n^" on "a\n": match "\n"; PCRE expects no match
    0/1 conforming
    |}]
;;

let%expect_test "named groups, branch reset, and internal options" =
  check
    [ {|(?P<name>a)|}, "a", Match "a"
    ; {|(?'name'a)|}, "a", Match "a"
    ; {|(?|(a)|(b))|}, "b", Match "b"
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
    "(?P<name>a)" on "a": parse error; PCRE expects match "a"
    "(?'name'a)" on "a": parse error; PCRE expects match "a"
    "(?|(a)|(b))" on "b": parse error; PCRE expects match "b"
    "(?i)a" on "A": parse error; PCRE expects match "A"
    "(?i:a)b" on "Ab": parse error; PCRE expects match "Ab"
    "(?i)a(?-i)b" on "Ab": parse error; PCRE expects match "Ab"
    "(?m)^a" on "x\na": parse error; PCRE expects match "a"
    "(?s)." on "\n": parse error; PCRE expects match "\n"
    "(?U)a+" on "aaa": parse error; PCRE expects match "a"
    "(?n)(a)" on "a": parse error; PCRE expects match "a"
    "(?x) a # comment\n b" on "ab": parse error; PCRE expects match "ab"
    "(?xx)[ a ]" on "a": parse error; PCRE expects match "a"
    "(?J)(?<a>x)|(?<a>y)" on "y": parse error; PCRE expects match "y"
    "(?)a" on "a": parse error; PCRE expects match "a"
    "(?^)a" on "a": parse error; PCRE expects match "a"
    "(?aT)[[:digit:]]" on "1": parse error; PCRE expects match "1"
    0/16 conforming
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
    "(?[ [a-z] & [^aeiou] ])" on "b": parse error; PCRE expects match "b"
    "(?[ [a-z] - [aeiou] ])" on "b": parse error; PCRE expects match "b"
    "(?[ [a] | [b] ])" on "b": parse error; PCRE expects match "b"
    "(?[ [a] + [b] ])" on "b": parse error; PCRE expects match "b"
    "(?[ [ab] ^ [bc] ])" on "a": parse error; PCRE expects match "a"
    "(?[ ![a] ])" on "b": parse error; PCRE expects match "b"
    0/6 conforming
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
    "a(?=b)" on "ab": parse error; PCRE expects match "a"
    "a(?!b)" on "ac": parse error; PCRE expects match "a"
    "(?<=a)b" on "ab": parse error; PCRE expects match "b"
    "(?<!a)b" on "cb": parse error; PCRE expects match "b"
    "(?<=ab?)c" on "abc": parse error; PCRE expects match "c"
    "(*pla:a)a" on "a": parse error; PCRE expects match "a"
    "(*negative_lookahead:b)a" on "a": parse error; PCRE expects match "a"
    "(*plb:a)b" on "ab": parse error; PCRE expects match "b"
    "(*negative_lookbehind:a)b" on "cb": parse error; PCRE expects match "b"
    "(?*a)a" on "a": parse error; PCRE expects match "a"
    "(?<*a)b" on "ab": parse error; PCRE expects match "b"
    "(*napla:a)a" on "a": parse error; PCRE expects match "a"
    "(*naplb:a)b" on "ab": parse error; PCRE expects match "b"
    "(?>a|ab)c" on "abc": parse error; PCRE expects no match
    "(*atomic:a|ab)c" on "abc": parse error; PCRE expects no match
    "a*+a" on "aaa": parse error; PCRE expects no match
    "a++a" on "aaa": parse error; PCRE expects no match
    "a?+a" on "a": parse error; PCRE expects no match
    "a{1,2}+a" on "aa": parse error; PCRE expects no match
    "(?!)" on "": parse error; PCRE expects no match
    "(?=)" on "": parse error; PCRE expects match ""
    0/21 conforming
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
    "(a)?(?(1)b|c)" on "ab": parse error; PCRE expects match "ab"
    "(?<a>a)?(?(<a>)b|c)" on "c": parse error; PCRE expects match "c"
    "(a)?(?(-1)b|c)" on "ab": parse error; PCRE expects match "ab"
    "(?(?=a)a|b)" on "a": parse error; PCRE expects match "a"
    "(?(VERSION>=10.0)a|b)" on "a": parse error; PCRE expects match "a"
    "(?(R)b|a)" on "a": parse error; PCRE expects match "a"
    "(?(DEFINE)(?<a>a))(?&a)" on "a": parse error; PCRE expects match "a"
    "(a)(?1)" on "aa": parse error; PCRE expects match "aa"
    "(a)(?-1)" on "aa": parse error; PCRE expects match "aa"
    "(?+1)(a)" on "aa": parse error; PCRE expects match "aa"
    "(?<a>a)(?P>a)" on "aa": parse error; PCRE expects match "aa"
    "(a)\\g<1>" on "aa": parse error; PCRE expects match "aa"
    "(?<a>a)\\g'a'" on "aa": parse error; PCRE expects match "aa"
    "a(?R)?b" on "aabb": parse error; PCRE expects match "aabb"
    "a(?0)?b" on "aabb": parse error; PCRE expects match "aabb"
    "(?<a>a)(?&a(<a>))" on "aa": parse error; PCRE expects match "aa"
    "(a)(*scan_substring:(1)a)" on "a": parse error; PCRE expects match "a"
    "(a)(*scs:(1)a)" on "a": parse error; PCRE expects match "a"
    0/18 conforming
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
    "a(*FAIL)|b" on "ab": parse error; PCRE expects match "b"
    "a(*F)|b" on "ab": parse error; PCRE expects match "b"
    "a(*ACCEPT)b" on "a": parse error; PCRE expects match "a"
    "a(*MARK:here)" on "a": parse error; PCRE expects match "a"
    "a(*:here)" on "a": parse error; PCRE expects match "a"
    "a(*COMMIT)b|ac" on "ac": parse error; PCRE expects no match
    "a(*PRUNE)b|ac" on "ac": parse error; PCRE expects no match
    "a(*SKIP)(*F)|." on "ab": parse error; PCRE expects match "b"
    "a(*THEN)b|ac" on "ac": parse error; PCRE expects match "ac"
    "a(?C)b" on "ab": parse error; PCRE expects match "ab"
    "a(?C1)b" on "ab": parse error; PCRE expects match "ab"
    "a(?C\"note\")b" on "ab": parse error; PCRE expects match "ab"
    "(*NOTEMPTY)a*" on "b": parse error; PCRE expects no match
    "(*NOTEMPTY_ATSTART)a*" on "b": parse error; PCRE expects match ""
    "(*NO_AUTO_POSSESS)a" on "a": parse error; PCRE expects match "a"
    "(*NO_DOTSTAR_ANCHOR)a" on "a": parse error; PCRE expects match "a"
    "(*NO_JIT)a" on "a": parse error; PCRE expects match "a"
    "(*NO_START_OPT)a" on "a": parse error; PCRE expects match "a"
    "(*LIMIT_DEPTH=100)a" on "a": parse error; PCRE expects match "a"
    "(*LIMIT_RECURSION=100)a" on "a": parse error; PCRE expects match "a"
    "(*LIMIT_HEAP=100)a" on "a": parse error; PCRE expects match "a"
    "(*LIMIT_MATCH=100)a" on "a": parse error; PCRE expects match "a"
    "(*CR)." on "\r": parse error; PCRE expects no match
    "(*LF)." on "\n": parse error; PCRE expects no match
    "(*CRLF)." on "\n": parse error; PCRE expects match "\n"
    "(*ANYCRLF)." on "\r": parse error; PCRE expects no match
    "(*ANY)." on "\133": parse error; PCRE expects no match
    "(*NUL)." on "\000": parse error; PCRE expects no match
    "(*BSR_ANYCRLF)\\R" on "\133": parse error; PCRE expects no match
    "(*BSR_UNICODE)\\R" on "\133": parse error; PCRE expects match "\133"
    0/30 conforming
    |}]
;;

let%expect_test "invalid byte syntax must not silently compile" =
  check
    [ {|\x{}|}, "", Parse_error
    ; {|\o{}|}, "", Parse_error
    ; {|\x{100}|}, "", Parse_error
    ; {|\o{400}|}, "", Parse_error
    ; {|\x{ffffffffffffffffffffffffffffffff}|}, "", Parse_error
    ; {|\o{77777777777777777777777777777777}|}, "", Parse_error
    ; {|\xZ|}, "", Parse_error
    ; {|\c|}, "", Parse_error
    ; {|[\R]|}, "", Parse_error
    ; {|[\N]|}, "", Parse_error
    ; {|[z-a]|}, "z", Parse_error
    ; {|[a-\d]|}, "a", Parse_error
    ; {|[\d-a]|}, "a", Parse_error
    ; {|[[.a.]]|}, "a", Parse_error
    ; {|[[=a=]]|}, "a", Parse_error
    ; {|^*|}, "", Parse_error
    ; {|(?<a>x)(?<a>y)|}, "xy", Parse_error
    ];
  [%expect
    {|
    "[z-a]" on "z": match "z"; PCRE expects parse error
    "[a-\\d]" on "a": match "a"; PCRE expects parse error
    "[\\d-a]" on "a": match "a"; PCRE expects parse error
    "[[.a.]]" on "a": match "a"; PCRE expects parse error
    "[[=a=]]" on "a": not supported; PCRE expects parse error
    "^*" on "": match ""; PCRE expects parse error
    "(?<a>x)(?<a>y)" on "xy": match "xy"; PCRE expects parse error
    10/17 conforming
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
    group count: 2 (PCRE: 3)
    group 1: "b" (PCRE: unset)
    |}]
;;

let%expect_test "inline comments are lexical, not empty atoms" =
  check
    [ {|a(?#note)*|}, "aaa", Match "aaa"
    ; {|(?#note)*|}, "", Parse_error
    ; {|a+(?#note)?|}, "aaa", Match "a"
    ; {|a(?#note){2}|}, "aa", Match "aa"
    ];
  [%expect
    {|
    "a(?#note)*" on "aaa": match "a"; PCRE expects match "aaa"
    "(?#note)*" on "": match ""; PCRE expects parse error
    "a+(?#note)?" on "aaa": match "aaa"; PCRE expects match "a"
    "a(?#note){2}" on "aa": match "a"; PCRE expects match "aa"
    0/4 conforming
    |}]
;;
