open Import

(* Tests based on description of Perl regular expressions given at
   http://www.perl.com/CPAN-local/doc/manual/html/pod/perlre.html *)

let re ?opts s = Format.printf "%a@." Re.pp (Re.Perl.re ?opts s)

let try_parse ?opts s =
  try
    ignore (Re.Perl.re ?opts s);
    print_endline "Prased successfully"
  with
  | Re.Perl.Parse_error -> print_endline "Parse error"
  | Re.Perl.Not_supported -> print_endline "Not supported"
;;

let%expect_test "escaping meta characters" =
  re "\\^";
  [%expect {| (Set 94) |}];
  re "\\.";
  [%expect {| (Set 46) |}];
  re "\\$";
  [%expect {| (Set 36) |}];
  re "\\|";
  [%expect {| (Set 124) |}];
  re "\\(";
  [%expect {| (Set 40) |}];
  re "\\)";
  [%expect {| (Set 41) |}];
  re "\\[";
  [%expect {| (Set 91) |}];
  re "\\]";
  [%expect {| (Set 93) |}];
  re "\\*";
  [%expect {| (Set 42) |}];
  re "\\+";
  [%expect {| (Set 43) |}];
  re "\\?";
  [%expect {| (Set 63) |}];
  re "\\\\";
  [%expect {| (Set 92) |}]
;;

let%expect_test "basic metacharacters" =
  re "^";
  [%expect {| Beg_of_str |}];
  re ".";
  [%expect {| (Set 0-9, 11-255) |}];
  re "$";
  [%expect {| Last_end_of_line |}];
  re "a|b";
  [%expect {| (Alternative (Set 97)(Set 98)) |}];
  re "aa|bb";
  [%expect {| (Alternative (Sequence (Set 97)(Set 97))(Sequence (Set 98)(Set 98))) |}];
  re "(a)";
  [%expect {| (Group (Set 97)) |}];
  re "(a|b)c";
  [%expect {| (Sequence (Group (Alternative (Set 97)(Set 98)))(Set 99)) |}];
  re "[ab]";
  [%expect {| (Alternative (Set 98)(Set 97)) |}];
  re "[a-z]";
  [%expect {| (Set 97-122) |}];
  re "[a-z$%.]";
  [%expect {| (Alternative (Set 46)(Set 37)(Set 36)(Set 97-122)) |}];
  re "[-az]";
  [%expect {| (Alternative (Set 122)(Set 97)(Set 45)) |}];
  re "[az-]";
  [%expect {| (Alternative (Set 122)(Set 45)(Set 97)) |}];
  re "[a\\-z]";
  [%expect {| (Alternative (Set 122)(Set 45)(Set 97)) |}];
  re "[]a]";
  [%expect {| (Alternative (Set 97)(Set 93)) |}];
  re "[]-]";
  [%expect {| (Alternative (Set 93)(Set 45)) |}];
  re "[a^]";
  [%expect {| (Alternative (Set 94)(Set 97)) |}];
  re "[^a-z]";
  [%expect {| (Complement (Set 97-122)) |}];
  re "[^a-z$]";
  [%expect {| (Complement (Set 36)(Set 97-122)) |}];
  re "[a-\\sz]";
  [%expect {| (Alternative (Set 122)(Set 97)(Set 45)(Set 9-13, 32)) |}]
;;

let%expect_test "greedy quantifiers" =
  re "a*";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0)) |}];
  re "a+";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 1)) |}];
  re "a?";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0 1)) |}];
  re "a{10}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10 10)) |}];
  re "a{10,}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10)) |}];
  re "a{10,12}";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 10 12)) |}]
;;

let%expect_test "non-greedy quantifiers" =
  re "a*?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0)) |}];
  re "a+?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 1)) |}];
  re "a??";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0 1)) |}];
  re "a{10}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10 10)) |}];
  re "a{10,}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10)) |}];
  re "a{10,12}?";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 10 12)) |}]
;;

let%expect_test "character sets" =
  re "\\w";
  [%expect
    {|
    (Alternative
       (Set 48-57, 65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255)
       (Set 95)) |}];
  re "\\W";
  [%expect
    {|
    (Complement
       (Set 48-57, 65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255)
       (Set 95)) |}];
  re "\\s";
  [%expect {| (Set 9-13, 32) |}];
  re "\\S";
  [%expect {| (Complement (Set 9-13, 32)) |}];
  re "\\d";
  [%expect {| (Set 48-57) |}];
  re "\\D";
  [%expect {| (Complement (Set 48-57)) |}]
;;

let%expect_test "zero-width assertions" =
  re "\\b";
  [%expect {| (Alternative Beg_of_wordEnd_of_word) |}];
  re "\\B";
  [%expect {| Not_bound |}];
  re "\\A";
  [%expect {| Beg_of_str |}];
  re "\\Z";
  [%expect {| Last_end_of_line |}];
  re "\\z";
  [%expect {| End_of_str |}];
  re "\\G";
  [%expect {| Start |}]
;;

let%expect_test "strict end-of-string escape" =
  let r = Re.Perl.compile_pat {|a\z|} in
  assert (Re.execp r "a");
  assert (Re.execp r "ba");
  assert (not (Re.execp r "ab"));
  assert (not (Re.execp r "a\n"));
  [%expect {||}]
;;

let%expect_test "options" =
  re ~opts:[ `Anchored ] "a";
  [%expect {| (Sequence Start(Set 97)) |}];
  re ~opts:[ `Caseless ] "b";
  [%expect {| (No_case (Set 98)) |}];
  re ~opts:[ `Dollar_endonly ] "$";
  [%expect {| End_of_str |}];
  re ~opts:[ `Dollar_endonly; `Multiline ] "$";
  [%expect {| End_of_line |}];
  re ~opts:[ `Dotall ] ".";
  [%expect {| (Set 0-255) |}];
  re ~opts:[ `Multiline ] "^";
  [%expect {| Beg_of_line |}];
  re ~opts:[ `Multiline ] "$";
  [%expect {| End_of_line |}];
  re ~opts:[ `Ungreedy ] "a*";
  [%expect {| (Sem_greedy Non_greedy (Repeat (Set 97) 0)) |}];
  re ~opts:[ `Ungreedy ] "a*?";
  [%expect {| (Sem_greedy Greedy (Repeat (Set 97) 0)) |}]
;;

let%expect_test "clustering" =
  re "(?:a)";
  [%expect {| (Set 97) |}];
  re "(?:a|b)c";
  [%expect {| (Sequence (Alternative (Set 97)(Set 98))(Set 99)) |}]
;;

let%expect_test "comment" =
  re "a(?#comment)b";
  [%expect {| (Sequence (Set 97)(Set 98)) |}];
  try_parse "(?#";
  [%expect {| Parse error |}]
;;

let%expect_test "backrefs" =
  try_parse "\\0";
  [%expect {| Prased successfully |}]
;;

let%expect_test "ordinary characters" =
  re "a";
  [%expect {| (Set 97) |}]
;;

let%expect_test "concacentation" =
  re "ab";
  [%expect {| (Sequence (Set 97)(Set 98)) |}]
;;

let%expect_test "sets in classes" =
  re "[a\\s]";
  [%expect {| (Alternative (Set 9-13, 32)(Set 97)) |}]
;;

let%expect_test "fixed bug" =
  (try ignore (Re.compile (Re.Perl.re "(.*?)(\\WPl|\\Bpl)(.*)")) with
   | _ -> failwith "bug in Re.handle_case");
  [%expect {||}]
;;

let outcome pattern subject =
  match Re.Perl.re_result pattern with
  | Error `Parse_error -> "parse error"
  | Error `Not_supported -> "not supported"
  | Ok re ->
    (match Re.exec_opt (Re.compile re) subject with
     | None -> "no match"
     | Some groups -> Printf.sprintf "match %S" (Re.Group.get groups 0))
;;

let show pattern subject =
  Printf.printf "%S on %S: %s\n" pattern subject (outcome pattern subject)
;;

let%expect_test "open lower bound quantifiers" =
  List.iter
    [ {|^a{,2}$|}, "aa"; {|^a{,2}?$|}, "aa"; {|^a{,0}$|}, "" ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "^a{,2}$" on "aa": match "aa"
    "^a{,2}?$" on "aa": match "aa"
    "^a{,0}$" on "": match ""
    |}]
;;

let%expect_test "non-ASCII bytes in capture-group names" =
  (* Names are byte strings used for lookup and never affect matching, so
     non-ASCII bytes are carried through verbatim rather than validated as
     Unicode identifier characters. *)
  List.iter
    [ {|(?<ñ>a)|}, "a"; {|(?<ñ>a)|}, "b"; {|(?<naïve>b)|}, "b" ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "(?<\195\177>a)" on "a": match "a"
    "(?<\195\177>a)" on "b": no match
    "(?<na\195\175ve>b)" on "b": match "b"
    |}]
;;

let%expect_test "inline modifiers" =
  List.iter
    [ {|(?i)abc|}, "ABC"
    ; {|(?i)abc|}, "abc"
    ; {|(?-i)abc|}, "ABC"
    ; {|(?i:abc)d|}, "ABCd"
    ; {|(?i:abc)d|}, "ABCD"
    ; {|a(?i)b|}, "aB"
    ; {|(?s).|}, "\n"
    ; {|^b|}, "a\nb"
    ; {|(?m)^b|}, "a\nb"
    ; {|(?^i)abc|}, "ABC"
    ; {|(?i-m)abc|}, "ABC"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "(?i)abc" on "ABC": match "ABC"
    "(?i)abc" on "abc": match "abc"
    "(?-i)abc" on "ABC": no match
    "(?i:abc)d" on "ABCd": match "ABCd"
    "(?i:abc)d" on "ABCD": no match
    "a(?i)b" on "aB": match "aB"
    "(?s)." on "\n": match "\n"
    "^b" on "a\nb": no match
    "(?m)^b" on "a\nb": match "b"
    "(?^i)abc" on "ABC": match "ABC"
    "(?i-m)abc" on "ABC": match "ABC"
    |}]
;;

let%expect_test "inline no-capture modifier" =
  (match Re.Perl.re_result {|(?n)(a)(b)|} with
   | Error `Parse_error -> print_endline "parse error"
   | Error `Not_supported -> print_endline "not supported"
   | Ok re ->
     Array.iter (Printf.printf "%S\n") (Re.Group.all (Re.exec (Re.compile re) "ab")));
  [%expect {| "ab" |}]
;;

let%expect_test "unsupported inline modifiers" =
  List.iter
    [ {|(?x)a|}
    ; {|(?xx)a|}
    ; {|(?a)a|}
    ; {|(?aa)a|}
    ; {|(?u)a|}
    ; {|(?l)a|}
    ; {|(?d)a|}
    ; {|(?p)a|}
    ]
    ~f:(fun pattern -> Printf.printf "%S: %s\n" pattern (outcome pattern "a"));
  [%expect
    {|
    "(?x)a": not supported
    "(?xx)a": not supported
    "(?a)a": not supported
    "(?aa)a": not supported
    "(?u)a": not supported
    "(?l)a": not supported
    "(?d)a": not supported
    "(?p)a": not supported
    |}]
;;

let%expect_test "case-modification escapes" =
  List.iter
    [ {|\lFOO|}, "fOO"
    ; {|\lFOO|}, "FOO"
    ; {|\uFOO|}, "FOO"
    ; {|\LFOO\E|}, "foo"
    ; {|\LFOO\E|}, "FOO"
    ; {|\Ufoo\E|}, "FOO"
    ; {|\Ffoo\E|}, "foo"
    ; {|\UFOO\Ebar|}, "FOObar"
    ; {|\L[A-Z]|}, "q"
    ; {|[\LA]|}, "a"
    ; {|\L\x41|}, "A"
    ; {|\u\x61|}, "a"
    ; {|\Q\Ufoo\E|}, "FOO"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "\\lFOO" on "fOO": parse error
    "\\lFOO" on "FOO": parse error
    "\\uFOO" on "FOO": parse error
    "\\LFOO\\E" on "foo": parse error
    "\\LFOO\\E" on "FOO": parse error
    "\\Ufoo\\E" on "FOO": parse error
    "\\Ffoo\\E" on "foo": parse error
    "\\UFOO\\Ebar" on "FOObar": parse error
    "\\L[A-Z]" on "q": parse error
    "[\\LA]" on "a": parse error
    "\\L\\x41" on "A": parse error
    "\\u\\x61" on "a": parse error
    "\\Q\\Ufoo\\E" on "FOO": no match
    |}]
;;
