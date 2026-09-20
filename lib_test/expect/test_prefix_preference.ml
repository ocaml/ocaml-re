open Import
open Re

let%expect_test "common variable-length prefixes preserve first-alternative preference" =
  List.iter
    [ "a*?b|a*?a", "(a*?b)|(a*?a)"
    ; "a*a|a*b", "(a*a)|(a*b)"
    ; "(?:a|aa)b|(?:a|aa)a", "((?:a|aa)b)|((?:a|aa)a)"
    ]
    ~f:(fun (pattern, reference) ->
      let matched pattern =
        let groups = exec (Perl.compile_pat pattern) "aab" in
        Group.get groups 0
      in
      Format.printf
        "%S on \"aab\": %S; unfactored: %S@."
        pattern
        (matched pattern)
        (matched reference));
  [%expect
    {|
    "a*?b|a*?a" on "aab": "a"; unfactored: "aab"
    "a*a|a*b" on "aab": "aab"; unfactored: "aa"
    "(?:a|aa)b|(?:a|aa)a" on "aab": "aa"; unfactored: "aab"
    |}]
;;
