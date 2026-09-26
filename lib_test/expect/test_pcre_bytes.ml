open Import
module Check = Pcre_test_helpers

let%expect_test "numeric byte escapes denote exactly one byte, also in classes" =
  List.iter
    [ ("short hex", fun code -> Printf.sprintf {|\x%02x|} code)
    ; ("braced hex", fun code -> Printf.sprintf {|\x{%x}|} code)
    ; ("zero-padded hex", fun code -> Printf.sprintf {|\x{0000%X}|} code)
    ; ("braced octal", fun code -> Printf.sprintf {|\o{%o}|} code)
    ; ("zero-padded octal", fun code -> Printf.sprintf {|\o{0000%o}|} code)
    ; ("short octal", fun code -> Printf.sprintf {|\%03o|} code)
    ]
    ~f:(fun (label, spell) ->
      print_endline label;
      let checks = Check.create () in
      for code = 0 to 255 do
        let pattern = spell code in
        List.iter
          [ pattern; "[" ^ pattern ^ "]" ]
          ~f:(fun pattern ->
            Check.check
              checks
              (Check.text pattern)
              ~expected:(Check.text (String.make 1 (Char.chr code)))
              (Check.byte_set ~extra:[ String.make 2 (Char.chr code) ] pattern))
      done;
      Check.finish checks);
  [%expect
    {|
    short hex
    512 checks; 0 differences
    braced hex
    512 checks; 0 differences
    zero-padded hex
    512 checks; 0 differences
    braced octal
    512 checks; 0 differences
    zero-padded octal
    512 checks; 0 differences
    short octal
    512 checks; 0 differences
    |}]
;;

let%expect_test "byte escapes in mixed classes and ranges" =
  let checks = Check.create () in
  List.iter
    [ {|\a|}, "\007"
    ; {|[\a\e\f]|}, "\007\012\027"
    ; {|\n|}, "\n"
    ; {|[\n\r\t]|}, "\t\n\r"
    ; {|[\cA-\cZ]|}, Check.bytes_where (fun code -> 1 <= code && code <= 26)
    ; {|[\x41-\x5a]|}, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ; {|[\101-\132]|}, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ; {|[\0]|}, "\000"
    ; {|[\7]|}, "\007"
    ; {|[\8\9]|}, "89"
    ]
    ~f:(fun (pattern, expected) ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:(Check.text expected)
        (Check.byte_set pattern));
  Check.finish checks;
  [%expect {| 10 checks; 0 differences |}]
;;

let%expect_test "control escapes cover printable ASCII operands" =
  let checks = Check.create () in
  for code = 32 to 126 do
    let c = Char.chr code in
    let pattern = "\\c" ^ String.make 1 c in
    let expected =
      String.make 1 (Char.chr (Char.code (Char.uppercase_ascii c) lxor 64))
    in
    List.iter
      [ pattern; "[" ^ pattern ^ "]" ]
      ~f:(fun pattern ->
        Check.check
          checks
          (Check.text pattern)
          ~expected:(Check.text expected)
          (Check.byte_set ~extra:[ expected ^ expected ] pattern))
  done;
  Check.finish checks;
  [%expect {| 190 checks; 0 differences |}]
;;

let%expect_test "numeric escapes stop before non-digits and after their digit limit" =
  let checks = Check.create () in
  List.iter
    [ {|\0z|}, "\000z"
    ; {|\08|}, "\0008"
    ; {|\078|}, "\0078"
    ; {|\0113|}, "\t3"
    ; {|\11x|}, "\tx"
    ; {|\118|}, "\t8"
    ; {|\xAz|}, "\nz"
    ; {|\x414|}, "A4"
    ; {|[\11x]|}, "x"
    ; {|[\xAz]|}, "z"
    ; "\\x{\t41 }", "A"
    ; "\\o{ 101\t}", "A"
    ; "\\x{" ^ String.make 1000 '0' ^ "41}", "A"
    ]
    ~f:(fun (pattern, subject) ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:(Check.text subject)
        (Check.match_text ~whole:true pattern subject));
  Check.finish checks;
  [%expect {| 13 checks; 0 differences |}]
;;

let%expect_test "invalid numeric and control escapes are parse errors" =
  let checks = Check.create () in
  List.iter
    [ {|\x|}
    ; {|\xg|}
    ; {|\x{}|}
    ; {|\x{ }|}
    ; {|\x{a|}
    ; {|\x{1 2}|}
    ; {|\x{\t41 }|} (* A literal backslash-t is not brace whitespace. *)
    ; {|\o|}
    ; {|\o{}|}
    ; {|\o{8}|}
    ; {|\o{1 2}|}
    ; {|\o{1|}
    ; {|\x{100}|}
    ; {|\o{400}|}
    ; {|\400|}
    ; {|\777|}
    ; "\\x{" ^ String.make 1000 'f' ^ "}"
    ; "\\o{" ^ String.make 1000 '7' ^ "}"
    ; {|\c|}
    ; "\\c\n"
    ; "\\c\127"
    ; "\\c\255"
    ]
    ~f:(fun pattern ->
      List.iter
        [ pattern; "[" ^ pattern ^ "]" ]
        ~f:(fun pattern ->
          (* A closing bracket can itself be the operand of \c, but then the
           unterminated class must still be rejected. *)
          Check.check
            checks
            (Check.text pattern)
            ~expected:"parse error"
            (Check.parse_status pattern)));
  Check.finish checks;
  [%expect {| 44 checks; 0 differences |}]
;;

let%expect_test "ambiguous decimal references remain unsupported, never octal" =
  let checks = Check.create () in
  List.iter
    [ {|(a)\1|}
    ; {|\8|}
    ; {|\81|}
    ; {|\9|}
    ; String.concat "" (List.init ~len:11 ~f:(fun _ -> "(a)")) ^ {|\11|}
    ; String.concat "" (List.init ~len:11 ~f:(fun _ -> "(?<a>a)")) ^ {|\11|}
    ]
    ~f:(fun pattern ->
      Check.check
        checks
        (Check.text pattern)
        ~expected:"not supported"
        (Check.parse_status pattern));
  Check.finish checks;
  [%expect {| 6 checks; 0 differences |}]
;;
