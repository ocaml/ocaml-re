open Import
module Check = Pcre_test_helpers

let%expect_test "all named group spellings preserve numbering, names and offsets" =
  let checks = Check.create () in
  List.iter [ "(?<word_1>"; "(?P<word_1>"; "(?'word_1'" ] ~f:(fun opening ->
    let pattern = "(x)" ^ opening ^ "a(b)+)(c)" in
    let actual =
      Check.with_match pattern "!xabbc!" (fun re groups ->
        let named =
          match Re.Pcre.get_named_substring re "word_1" groups with
          | exception Not_found -> "unset"
          | s -> Check.text s
        in
        Printf.sprintf
          "names=%s pcre_names=%s captures=%s offsets=%s word_1=%s"
          (Check.list
             (fun (name, index) -> Printf.sprintf "%s=%d" name index)
             (Re.group_names re))
          (Check.array Check.text (Re.Pcre.names re))
          (Check.array Check.text (Re.Group.all groups))
          (Check.array Check.offset (Re.Group.all_offset groups))
          named)
    in
    Check.check
      checks
      (Check.text pattern)
      ~expected:
        {|names=[word_1=2] pcre_names=["word_1"] captures=["xabbc"; "x"; "abb"; "b"; "c"] offsets=[(1,6); (1,2); (2,5); (4,5); (5,6)] word_1="abb"|}
      actual);
  Check.finish checks;
  [%expect
    {|
    "(x)(?P<word_1>a(b)+)(c)": parse error; expected names=[word_1=2] pcre_names=["word_1"] captures=["xabbc"; "x"; "abb"; "b"; "c"] offsets=[(1,6); (1,2); (2,5); (4,5); (5,6)] word_1="abb"
    "(x)(?'word_1'a(b)+)(c)": parse error; expected names=[word_1=2] pcre_names=["word_1"] captures=["xabbc"; "x"; "abb"; "b"; "c"] offsets=[(1,6); (1,2); (2,5); (4,5); (5,6)] word_1="abb"
    3 checks; 2 differences
    |}]
;;

let%expect_test "named captures remain optional and work across alternatives" =
  let checks = Check.create () in
  let pattern = {|(?P<left>a)|(?'right'b)|} in
  let actual =
    Check.with_match pattern "b" (fun re groups ->
      Printf.sprintf
        "left=%s right=%s"
        (Check.option Check.text (Re.Pcre.get_named_substring_opt re "left" groups))
        (Check.option Check.text (Re.Pcre.get_named_substring_opt re "right" groups)))
  in
  Check.check checks (Check.text pattern) ~expected:{|left=unset right="b"|} actual;
  Check.finish checks;
  [%expect
    {|
    "(?P<left>a)|(?'right'b)": parse error; expected left=unset right="b"
    1 checks; 1 differences
    |}]
;;

let%expect_test "malformed names and unsupported group forms still fail" =
  let checks = Check.create () in
  List.iter
    [ {|(?P<>a)|}
    ; {|(?P<1a>a)|}
    ; {|(?P<a-b>a)|}
    ; {|(?P<a'a)|}
    ; {|(?'a>a)|}
    ; {|(?P<a>|}
    ; {|(?Pa)|}
    ; {|(?<=a)b|}
    ; {|(?<!a)b|}
    ; {|(?P=a)|}
    ; {|(?P>a)|}
    ]
    ~f:(fun pattern ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:"parse error"
        (Check.parse_status pattern));
  Check.finish checks;
  [%expect {| 11 checks; 0 differences |}]
;;

let%expect_test "word boundary aliases are zero-width and not character classes" =
  let checks = Check.create () in
  let pattern = {|[[:<:]]cat[[:>:]]|} in
  List.iter
    [ "cat", true; "!cat!", true; "catfish", false; "scat", false; "_cat", false ]
    ~f:(fun (subject, expected) ->
      Check.check
        checks
        (Printf.sprintf "%S on %S" pattern subject)
        ~expected:(if expected then Check.text "cat" else "no match")
        (Check.match_text pattern subject));
  List.iter
    [ pattern, "!cat!", "(1,4)"
    ; "[[:<:]]", "!a", "(1,1)"
    ; "[[:>:]]", "a!", "(1,1)"
    ; "[[:<:]]", "", "no match"
    ; "[[:>:]]", "", "no match"
    ]
    ~f:(fun (pattern, subject, expected) ->
      Check.check
        checks
        (Printf.sprintf "%S on %S offsets" pattern subject)
        ~expected
        (Check.with_match pattern subject (fun _ groups ->
           Check.offset (Re.Group.offset groups 0))));
  List.iter [ "[[:<:]a]"; "[a[:>:]]" ] ~f:(fun pattern ->
    Check.check
      checks
      (Check.text pattern)
      ~expected:"parse error"
      (Check.parse_status pattern));
  Check.finish checks;
  [%expect
    {|
    "[[:<:]]cat[[:>:]]" on "cat": parse error; expected "cat"
    "[[:<:]]cat[[:>:]]" on "!cat!": parse error; expected "cat"
    "[[:<:]]cat[[:>:]]" on "catfish": parse error; expected no match
    "[[:<:]]cat[[:>:]]" on "scat": parse error; expected no match
    "[[:<:]]cat[[:>:]]" on "_cat": parse error; expected no match
    "[[:<:]]cat[[:>:]]" on "!cat!" offsets: parse error; expected (1,4)
    ... 4 more differences
    12 checks; 10 differences
    |}]
;;
