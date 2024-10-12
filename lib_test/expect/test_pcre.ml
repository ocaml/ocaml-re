open Import
module Pcre = Re_private.Pcre

let test re s =
  match Pcre.re re with
  | exception _ -> Format.printf "failed to parse@."
  | re -> t re s
;;

let%expect_test "quoted strings" =
  test {|\Qfoo\E|} "foo";
  [%expect {| (Group (foo (0 3))) |}];
  test {|\Qbar|} "";
  [%expect {| failed to parse |}];
  test {|\Qbaz\|} "";
  [%expect {| failed to parse |}];
  test {|\Qba\Xz\E|} {|ba\Xz|};
  [%expect {| (Group (ba\Xz (0 5))) |}]
;;

let%expect_test "octal" =
  test {|\025|} (String.make 1 '\o025');
  [%expect {| (Group ( (0 1))) |}];
  test {|\999|} "";
  [%expect {| failed to parse |}];
  test {|\111|} (String.make 1 '\o111');
  [%expect {| (Group (I (0 1))) |}]
;;

let%expect_test "\\x and \\o form" =
  test {|\o{111}|} (String.make 1 '\o111');
  [%expect {| <None> |}];
  test {|\o{111|} "";
  [%expect {| failed to parse |}];
  test {|\x{ff}|} (String.make 1 '\xff');
  [%expect {| (Group (ÿ (0 1))) |}];
  test {|\x{ff|} "";
  [%expect {| failed to parse |}]
;;

let%expect_test "substitute" =
  let open Pcre in
  let substitute ~rex ~subst s = substitute ~rex ~subst s |> print_endline in
  let rex = regexp "[a-zA-Z]+" in
  let subst = String.capitalize_ascii in
  substitute ~rex ~subst " hello world; I love chips!";
  [%expect {| Hello World; I Love Chips! |}];
  substitute ~rex:re_empty ~subst:(fun _ -> "a") "";
  [%expect {| a |}];
  substitute ~rex:(regexp "a*") ~subst:(fun _ -> "*") "cat";
  [%expect {| *c*t* |}];
  let rex = regexp "^ *" in
  substitute ~rex ~subst:(fun _ -> "A ") "test";
  [%expect {| A test |}]
;;

let%expect_test "test_blank_class" =
  let re = Re.Perl.compile_pat "\\d[[:blank:]]\\d[[:blank:]]+[a-z]" in
  let successes = [ "1 2  a"; "2\t3 z"; "9\t0 \t a" ] in
  let failures = [ ""; "123"; "  "; "1 3z" ] in
  List.iter successes ~f:(fun s -> printf "String %S should match %b\n" s (Re.execp re s));
  [%expect
    {|
    String "1 2  a" should match true
    String "2\t3 z" should match true
    String "9\t0 \t a" should match true |}];
  List.iter failures ~f:(fun s ->
    printf "String %S should not match %b\n" s (Re.execp re s));
  [%expect
    {|
    String "" should not match false
    String "123" should not match false
    String "  " should not match false
    String "1 3z" should not match false |}]
;;

let%expect_test "named groups" =
  let open Pcre in
  let rex = regexp "(?<many_x>x+)" in
  let s = exec ~rex "testxxxyyy" in
  print_endline (get_named_substring rex "many_x" s);
  [%expect {| xxx |}]
;;

let%expect_test "quote" =
  let test s = Printf.printf "%S\n" (Re.Pcre.quote s) in
  test "";
  [%expect {| "" |}];
  test "\000";
  [%expect {| "\000" |}];
  test "";
  [%expect {| "" |}];
  test (String.init (126 - 32) (fun x -> Char.chr (x + 32)));
  [%expect
    {xxx| " !\"#\\$%&'\\(\\)\\*\\+,-\\./0123456789:;<=>\\?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{\\|}" |xxx}];
  let b = Buffer.create 100 in
  for i = 0 to 255 do
    let c = Char.chr i in
    let s = Pcre.quote (String.make 1 c) in
    if String.length s > 1 then Buffer.add_char b c
  done;
  let b = Buffer.contents b in
  Printf.printf "%S\n" b;
  [%expect {xxx| "$()*+.?[\\^{|" |xxx}]
;;
