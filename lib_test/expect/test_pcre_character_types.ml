open Import
module Check = Pcre_test_helpers

let%expect_test "horizontal and vertical byte sets and their complements" =
  let checks = Check.create () in
  List.iter
    [ 'h', "\t \160"; 'v', "\n\011\012\r\133" ]
    ~f:(fun (escape, members) ->
      List.iter [ false; true ] ~f:(fun complement ->
        let escape = if complement then Char.uppercase_ascii escape else escape in
        let pattern = "\\" ^ String.make 1 escape in
        List.iter
          [ pattern, false; "[" ^ pattern ^ "]", false; "[^" ^ pattern ^ "]", true ]
          ~f:(fun (pattern, negate) ->
            let expected =
              Check.bytes_where (fun code ->
                Bool.equal
                  (String.contains members (Char.chr code))
                  (Bool.equal complement negate))
            in
            Check.check
              checks
              (Check.text pattern)
              ~expected:(Check.text expected)
              (Check.byte_set pattern))));
  Check.check
    checks
    {|[\h\v]|}
    ~expected:(Check.text "\t\n\011\012\r \133\160")
    (Check.byte_set {|[\h\v]|});
  Check.finish checks;
  [%expect {xxx| 13 checks; 0 differences |xxx}]
;;

let%expect_test "word complements agree inside and outside classes" =
  let checks = Check.create () in
  let word = Re.compile (Re.whole_string (Re.Pcre.re {|\w|})) in
  let is_word code = Re.execp word (String.make 1 (Char.chr code)) in
  List.iter
    [ ({|\W|}, fun code -> not (is_word code))
    ; ({|[\W]|}, fun code -> not (is_word code))
    ; {|[^\W]|}, is_word
    ; ({|[^\W_]|}, fun code -> is_word code && code <> Char.code '_')
    ]
    ~f:(fun (pattern, expected) ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:(Check.text (Check.bytes_where expected))
        (Check.byte_set pattern));
  Check.finish checks;
  [%expect {xxx| 4 checks; 0 differences |xxx}]
;;

let%expect_test "backslash C and N are independent of dotall" =
  let checks = Check.create () in
  List.iter [ []; [ `DOTALL ] ] ~f:(fun flags ->
    List.iter
      [ ({|\C|}, fun _ -> true); ({|\N|}, fun code -> code <> 10) ]
      ~f:(fun (pattern, expected) ->
        Check.check
          checks
          (Printf.sprintf "%S dotall=%b" pattern (flags <> []))
          ~expected:(Check.text (Check.bytes_where expected))
          (Check.byte_set ~flags pattern)));
  Check.check
    checks
    {|\N{2} on "ab"|}
    ~expected:(Check.text "ab")
    (Check.match_text ~whole:true {|\N{2}|} "ab");
  Check.check
    checks
    {|\N{2} on "a\n"|}
    ~expected:"no match"
    (Check.match_text ~whole:true {|\N{2}|} "a\n");
  List.iter [ {|[\C]|}; {|[\N]|}; {|[\R]|}; {|\N{U+0041}|} ] ~f:(fun pattern ->
    Check.check
      checks
      (Check.text pattern)
      ~expected:"parse error"
      (Check.parse_status pattern));
  Check.finish checks;
  [%expect {xxx| 10 checks; 0 differences |xxx}]
;;
