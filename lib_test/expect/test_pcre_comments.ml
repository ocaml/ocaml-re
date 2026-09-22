open Import
module Check = Pcre_test_helpers

let%expect_test "comments do not change grouping or quantifier binding" =
  let checks = Check.create () in
  List.iter
    [ {|a(?#note)*|}, "aaa", "aaa"
    ; {|a(?#note){2}|}, "aaa", "aa"
    ; {|a+(?#note)?|}, "aaa", "a"
    ; {|a(?#one)(?#two)+|}, "aaa", "aaa"
    ; {|a\Q\E(?#note)+|}, "aaa", "aaa"
    ; {|\Qab\E(?#note)+|}, "abbb", "abbb"
    ; {|a(?#not an escape: \Q)+|}, "aaa", "aaa"
    ; {|(?#start)(a(?#inner))|b(?#end)|}, "a", "a"
    ; {|(?#only)|}, "", ""
    ; {|\Q(?#literal)\E|}, "(?#literal)", "(?#literal)"
    ; {|[(?#class)]|}, "#", "#"
    ]
    ~f:(fun (pattern, subject, expected) ->
      Check.check
        checks
        (Printf.sprintf "%S on %S" pattern subject)
        ~expected:(Check.text expected)
        (Check.match_text pattern subject));
  let pattern = {|(a)(?#comment)(b)|} in
  Check.check
    checks
    (Check.text pattern)
    ~expected:{|["ab"; "a"; "b"]|}
    (Check.with_match pattern "ab" (fun _ groups ->
       Check.array Check.text (Re.Group.all groups)));
  Check.finish checks;
  [%expect {| 12 checks; 0 differences |}]
;;

let%expect_test "unterminated comments and quantifiers without an operand" =
  let checks = Check.create () in
  List.iter
    [ "(?#"; "a(?#note"; "(?#note)*"; "(?#note)+"; "(?#note){2}" ]
    ~f:(fun pattern ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:"parse error"
        (Check.parse_status pattern));
  Check.finish checks;
  [%expect {| 5 checks; 0 differences |}]
;;
