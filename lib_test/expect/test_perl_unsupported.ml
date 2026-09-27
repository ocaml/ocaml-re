open Import
open Re

let outcome pattern =
  match Perl.re_result pattern with
  | Ok re -> Format.asprintf "PARSED: %a" pp re
  | Error `Parse_error -> "Parse_error"
  | Error `Not_supported -> "Not_supported"
;;

let print_unsupported patterns =
  List.iter patterns ~f:(fun pattern ->
    Printf.printf "%S: %s\n" pattern (outcome pattern))
;;

let%expect_test "inline charset and free-spacing modifiers are not supported" =
  (* perlre: "x", "a", "d", "l", "u" and "p" control free spacing and the
     character set rules. [Re.Perl] implements i, m, s and n inline; the rest
     still raise. *)
  print_unsupported
    [ {|(?x)abc|}
    ; {|(?xx)abc|}
    ; {|(?a)abc|}
    ; {|(?aa)abc|}
    ; {|(?u)abc|}
    ; {|(?l)abc|}
    ; {|(?d)abc|}
    ; {|(?p)abc|}
    ];
  [%expect
    {|
    "(?x)abc": Not_supported
    "(?xx)abc": Not_supported
    "(?a)abc": Not_supported
    "(?aa)abc": Not_supported
    "(?u)abc": Not_supported
    "(?l)abc": Not_supported
    "(?d)abc": Not_supported
    "(?p)abc": Not_supported
    |}]
;;

let%expect_test "possessive quantifiers are not supported" =
  (* perlre: a trailing "+" makes a quantifier possessive. *)
  print_unsupported
    [ {|a*+|}; {|a++|}; {|a?+|}; {|a{2}+|}; {|a{2,}+|}; {|a{,3}+|}; {|a{2,3}+|} ];
  [%expect
    {|
    "a*+": Parse_error
    "a++": Parse_error
    "a?+": Parse_error
    "a{2}+": Parse_error
    "a{2,}+": Parse_error
    "a{,3}+": Parse_error
    "a{2,3}+": Parse_error
    |}]
;;

let%expect_test "backreferences are not supported" =
  (* perlre: "\1".."\9", "\g1", "\g{1}", "\g{-1}", "\g{name}", "\k<name>",
     "\k'name'", "\k{name}", and "(?P=name)" all match the text captured by an
     earlier group. [Re.Perl] rejects numeric backreferences with
     [Not_supported] and the remaining spellings with [Parse_error]. *)
  print_unsupported
    [ {|(a)\1|}
    ; {|(a)(b)\1\2|}
    ; {|(a)\g1|}
    ; {|(a)\g{1}|}
    ; {|(a)\g{-1}|}
    ; {|(?<n>a)\g{n}|}
    ; {|(?<n>a)\k<n>|}
    ; {|(?<n>a)\k'n'|}
    ; {|(?<n>a)\k{n}|}
    ; {|(?<n>a)(?P=n)|}
    ];
  [%expect
    {|
    "(a)\\1": Not_supported
    "(a)(b)\\1\\2": Not_supported
    "(a)\\g1": Parse_error
    "(a)\\g{1}": Parse_error
    "(a)\\g{-1}": Parse_error
    "(?<n>a)\\g{n}": Parse_error
    "(?<n>a)\\k<n>": Parse_error
    "(?<n>a)\\k'n'": Parse_error
    "(?<n>a)\\k{n}": Parse_error
    "(?<n>a)(?P=n)": Parse_error
    |}]
;;

let%expect_test "lookaround assertions and \\K are not supported" =
  (* perlre: "(?=)", "(?!)", "(?<=)", "(?<!)" are zero-width lookaround
     assertions, "\K" keeps text out of the overall match, and the
     "(*pla:)", "(*nla:)", "(*plb:)", "(*nlb:)" spellings are long aliases
     for them. *)
  print_unsupported
    [ {|a(?=b)|}
    ; {|a(?!b)|}
    ; {|(?<=a)b|}
    ; {|(?<!a)b|}
    ; {|a\Kb|}
    ; {|(*pla:a)|}
    ; {|(*nla:a)|}
    ; {|(*plb:a)|}
    ; {|(*nlb:a)|}
    ; {|(*positive_lookahead:a)|}
    ; {|(*negative_lookahead:a)|}
    ; {|(*positive_lookbehind:a)|}
    ; {|(*negative_lookbehind:a)|}
    ];
  [%expect
    {|
    "a(?=b)": Parse_error
    "a(?!b)": Parse_error
    "(?<=a)b": Parse_error
    "(?<!a)b": Parse_error
    "a\\Kb": Parse_error
    "(*pla:a)": Parse_error
    "(*nla:a)": Parse_error
    "(*plb:a)": Parse_error
    "(*nlb:a)": Parse_error
    "(*positive_lookahead:a)": Parse_error
    "(*negative_lookahead:a)": Parse_error
    "(*positive_lookbehind:a)": Parse_error
    "(*negative_lookbehind:a)": Parse_error
    |}]
;;

let%expect_test
    "atomic groups, branch reset, conditionals and recursion are not supported"
  =
  (* perlre: "(?>...)" and "(*atomic:...)" are independent subexpressions,
     "(?|...)" resets capture numbering per branch, "(?(cond)...)" is a
     conditional, and "(?1)", "(?-1)", "(?+1)", "(?R)", "(?0)", "(?&name)",
     "(?P>name)" recurse into a group. The "(?(DEFINE))" predicate and
     code-block conditions are also part of this family. *)
  print_unsupported
    [ {|(?>a)|}
    ; {|(*atomic:a)|}
    ; {|(?|a|b)|}
    ; {|(a)(?(1)b|c)|}
    ; {|(?(?=a)b|c)|}
    ; {|(?<n>a)(?(<n>)b|c)|}
    ; {|(a)(?(R)b|c)|}
    ; {|(a)(?(R1)b|c)|}
    ; {|(?<n>a)(?(R&n)b|c)|}
    ; {|(a)(?(DEFINE)(?<x>b))|}
    ; {|(a)(?(?{1})b|c)|}
    ; {|(a)(?1)|}
    ; {|(a)(?-1)|}
    ; {|(a)(?+1)(b)|}
    ; {|(a)(?R)|}
    ; {|(a)(?0)|}
    ; {|(?<n>a)(?&n)|}
    ; {|(?<n>a)(?P>n)|}
    ];
  [%expect
    {|
    "(?>a)": Parse_error
    "(*atomic:a)": Parse_error
    "(?|a|b)": Parse_error
    "(a)(?(1)b|c)": Parse_error
    "(?(?=a)b|c)": Parse_error
    "(?<n>a)(?(<n>)b|c)": Parse_error
    "(a)(?(R)b|c)": Parse_error
    "(a)(?(R1)b|c)": Parse_error
    "(?<n>a)(?(R&n)b|c)": Parse_error
    "(a)(?(DEFINE)(?<x>b))": Parse_error
    "(a)(?(?{1})b|c)": Parse_error
    "(a)(?1)": Parse_error
    "(a)(?-1)": Parse_error
    "(a)(?+1)(b)": Parse_error
    "(a)(?R)": Parse_error
    "(a)(?0)": Parse_error
    "(?<n>a)(?&n)": Parse_error
    "(?<n>a)(?P>n)": Parse_error
    |}]
;;

let%expect_test "embedded code blocks are not supported" =
  (* perlre: "(?{...})" executes Perl code, "(??{...})" compiles and matches
     the code's return value as a pattern, and "(*{...})" is the
     optimisation-friendly variant. In Perl literals these are accepted at
     compile time; interpolated patterns additionally need "use re 'eval'". *)
  print_unsupported [ {|a(?{ 1 })|}; {|a(??{ "b" })|}; {|a(*{ 1 })|} ];
  [%expect
    {|
    "a(?{ 1 })": Parse_error
    "a(??{ \"b\" })": Parse_error
    "a(*{ 1 })": Parse_error
    |}]
;;

let%expect_test "script runs and backtracking control verbs are not supported" =
  (* perlre: "(*script_run:...)" and "(*sr:...)" require all matched
     characters to share a Unicode script, "(*atomic_script_run:...)" and
     "(*asr:...)" add atomicity, and the "(*VERB)" family controls
     backtracking. All are rejected by [Re.Perl] because they start with
     "(*". *)
  print_unsupported
    [ {|(*script_run:a)|}
    ; {|(*sr:a)|}
    ; {|(*atomic_script_run:a)|}
    ; {|(*asr:a)|}
    ; {|a(*PRUNE)b|}
    ; {|a(*PRUNE:n)b|}
    ; {|a(*SKIP)b|}
    ; {|a(*SKIP:n)b|}
    ; {|a(*MARK:n)b|}
    ; {|a(*:n)b|}
    ; {|a(*THEN)b|}
    ; {|a(*COMMIT)b|}
    ; {|a(*FAIL)b|}
    ; {|a(*F)b|}
    ; {|a(*ACCEPT)b|}
    ; {|a(*ACCEPT:n)b|}
    ];
  [%expect
    {|
    "(*script_run:a)": Parse_error
    "(*sr:a)": Parse_error
    "(*atomic_script_run:a)": Parse_error
    "(*asr:a)": Parse_error
    "a(*PRUNE)b": Parse_error
    "a(*PRUNE:n)b": Parse_error
    "a(*SKIP)b": Parse_error
    "a(*SKIP:n)b": Parse_error
    "a(*MARK:n)b": Parse_error
    "a(*:n)b": Parse_error
    "a(*THEN)b": Parse_error
    "a(*COMMIT)b": Parse_error
    "a(*FAIL)b": Parse_error
    "a(*F)b": Parse_error
    "a(*ACCEPT)b": Parse_error
    "a(*ACCEPT:n)b": Parse_error
    |}]
;;

let%expect_test "Unicode properties and named characters are not supported" =
  (* perlrecharclass: "\p{...}" and "\P{...}" (with one-letter and compound
     forms) match Unicode properties. perlrebackslash: "\N{NAME}" and
     "\N{U+XXXX}" name a character or character sequence. [Re.Perl] is
     byte-oriented and rejects all of them. *)
  print_unsupported
    [ {|\pL|}
    ; {|\p{L}|}
    ; {|\p{Lu}|}
    ; {|\PL|}
    ; {|\P{L}|}
    ; {|\p{Thai}|}
    ; {|\p{gc=Number}|}
    ; {|\p{sc=Greek}|}
    ; {|\p{White_Space}|}
    ; {|[\p{L}]|}
    ; {|[\P{L}]|}
    ; {|\N{U+0041}|}
    ; {|\N{LATIN CAPITAL LETTER A}|}
    ; {|[\N{U+0041}]|}
    ];
  [%expect
    {|
    "\\pL": Parse_error
    "\\p{L}": Parse_error
    "\\p{Lu}": Parse_error
    "\\PL": Parse_error
    "\\P{L}": Parse_error
    "\\p{Thai}": Parse_error
    "\\p{gc=Number}": Parse_error
    "\\p{sc=Greek}": Parse_error
    "\\p{White_Space}": Parse_error
    "[\\p{L}]": Parse_error
    "[\\P{L}]": Parse_error
    "\\N{U+0041}": Parse_error
    "\\N{LATIN CAPITAL LETTER A}": Parse_error
    "[\\N{U+0041}]": Parse_error
    |}]
;;

let%expect_test
    "grapheme clusters, generic newlines and Unicode boundaries are not supported"
  =
  (* perlrebackslash: "\X" matches an extended grapheme cluster and "\R" a
     generic newline. "\b{...}"/"\B{...}" select a Unicode boundary rule:
     gcb/g (grapheme cluster), lb (line break), sb (sentence), wb (word). *)
  print_unsupported
    [ {|\X|}
    ; {|\R|}
    ; {|\R\n|}
    ; {|^\R$|}
    ; {|\b{gcb}|}
    ; {|\b{g}|}
    ; {|\b{lb}|}
    ; {|\b{sb}|}
    ; {|\b{wb}|}
    ; {|\B{wb}|}
    ];
  [%expect
    {|
    "\\X": Parse_error
    "\\R": Parse_error
    "\\R\\n": Parse_error
    "^\\R$": Parse_error
    "\\b{gcb}": Parse_error
    "\\b{g}": Parse_error
    "\\b{lb}": Parse_error
    "\\b{sb}": Parse_error
    "\\b{wb}": Parse_error
    "\\B{wb}": Parse_error
    |}]
;;

let%expect_test "extended bracketed character classes are not supported" =
  (* perlrecharclass: "(?[...])" is a bracketed class with set operators
     ("&", "+", "|", "-", "^", "!") that always runs under "/xx" rules. *)
  print_unsupported
    [ {|(?[ \p{Thai} & \p{Digit} ])|}; {|(?[ [a] + [b] ])|}; {|(?[[ a b ]])|} ];
  [%expect
    {|
    "(?[ \\p{Thai} & \\p{Digit} ])": Parse_error
    "(?[ [a] + [b] ])": Parse_error
    "(?[[ a b ]])": Parse_error
    |}]
;;

let%expect_test "code points above 0xFF are not supported" =
  (* Perl allows "\x{...}" and "\o{...}" to denote any Unicode code point.
     [Re.Perl] represents characters as bytes, so values above 255 are a
     parse error. *)
  print_unsupported [ {|\x{100}|}; {|\x{263B}|}; {|\o{400}|}; {|\x{10FFFF}|} ];
  [%expect
    {|
    "\\x{100}": Parse_error
    "\\x{263B}": Parse_error
    "\\o{400}": Parse_error
    "\\x{10FFFF}": Parse_error
    |}]
;;

let%expect_test "character classes and case folding lack Unicode semantics" =
  (* Perl's "/u" semantics: "\d" matches all Unicode decimal digits, "\s"
     includes U+00A0 NO-BREAK SPACE, "/i" folds É with é, and sharp s folds
     with the two-character sequence "ss". [Re.Perl] works on bytes, so none
     of these match. These are semantic gaps rather than syntax gaps: the
     patterns parse, they just cannot mean what they mean in Perl. *)
  let matches pattern subject = Re.execp (Perl.compile_pat pattern) subject in
  (* U+0663 ARABIC-INDIC DIGIT THREE, UTF-8: D9 A3 *)
  assert (not (matches {|\d|} "\xd9\xa3"));
  (* U+00A0 NO-BREAK SPACE, UTF-8: C2 A0 *)
  assert (not (matches {|\s|} "\xc2\xa0"));
  (* U+00C9 LATIN CAPITAL LETTER E WITH ACUTE vs U+00E9 *)
  let upper_e_acute = Perl.compile_pat ~opts:[ `Caseless ] "\xc3\x89" in
  assert (not (Re.execp upper_e_acute "\xc3\xa9"));
  (* U+00DF LATIN SMALL LETTER SHARP S folds to "ss" under Perl's /i *)
  let sharp_s = Perl.compile_pat ~opts:[ `Caseless ] "\xdf" in
  assert (not (Re.execp sharp_s "ss"));
  [%expect {||}]
;;
