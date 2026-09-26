open Import
open Re

let%expect_test "common variable-length prefixes preserve first-alternative preference" =
  (* A capturing wrapper prevents the original prefix-factoring pass, providing
     a reference with the same language and first-match preference. *)
  List.iter
    [ "a*?b|a*?a", "(a*?b)|(a*?a)", "aab"
    ; "a*a|a*b", "(a*a)|(a*b)", "aab"
    ; "(?:a|aa)b|(?:a|aa)a", "((?:a|aa)b)|((?:a|aa)a)", "aab"
    ; "a+?b|a+?a", "(a+?b)|(a+?a)", "aab"
    ; "a??b|a??a", "(a??b)|(a??a)", "ab"
    ; "a{0,2}?b|a{0,2}?a", "(a{0,2}?b)|(a{0,2}?a)", "aab"
    ; "a{1,3}?b|a{1,3}?a", "(a{1,3}?b)|(a{1,3}?a)", "aab"
    ; "(?:a|aa)+?b|(?:a|aa)+?a", "((?:a|aa)+?b)|((?:a|aa)+?a)", "aab"
    ; "(?:(?:a|aa))b|(?:(?:a|aa))a", "((?:(?:a|aa))b)|((?:(?:a|aa))a)", "aab"
    ]
    ~f:(fun (pattern, reference, input) ->
      let matched pattern =
        let groups = exec (Perl.compile_pat pattern) input in
        Group.get groups 0
      in
      Format.printf
        "%S on %S: %S; unfactored: %S@."
        pattern
        input
        (matched pattern)
        (matched reference));
  [%expect
    {|
    "a*?b|a*?a" on "aab": "aab"; unfactored: "aab"
    "a*a|a*b" on "aab": "aa"; unfactored: "aa"
    "(?:a|aa)b|(?:a|aa)a" on "aab": "aab"; unfactored: "aab"
    "a+?b|a+?a" on "aab": "aab"; unfactored: "aab"
    "a??b|a??a" on "ab": "ab"; unfactored: "ab"
    "a{0,2}?b|a{0,2}?a" on "aab": "aab"; unfactored: "aab"
    "a{1,3}?b|a{1,3}?a" on "aab": "aab"; unfactored: "aab"
    "(?:a|aa)+?b|(?:a|aa)+?a" on "aab": "aab"; unfactored: "aab"
    "(?:(?:a|aa))b|(?:(?:a|aa))a" on "aab": "aab"; unfactored: "aab"
    |}]
;;
