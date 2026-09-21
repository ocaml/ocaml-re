open Import
module Check = Pcre_test_helpers

let%expect_test "dollar permits one final LF by default, unlike end-only" =
  let checks = Check.create () in
  let show = function
    | None -> "no match"
    | Some offset -> Check.offset offset
  in
  let test opts pattern subject expected =
    let re = Re.Perl.compile_pat ~opts pattern in
    let actual = Option.map (fun g -> Re.Group.offset g 0) (Re.exec_opt re subject) in
    let opts =
      Check.list
        (function
          | `Dollar_endonly -> "Dollar_endonly"
          | `Multiline -> "Multiline"
          | _ -> "other")
        opts
    in
    Check.check
      checks
      (Printf.sprintf "%S on %S opts=%s" pattern subject opts)
      ~expected:(show expected)
      (show actual)
  in
  List.iter [ []; [ `Dollar_endonly ] ] ~f:(fun opts ->
    test opts "a$" "a" (Some (0, 1));
    test opts "a$" "a\nb" None;
    test opts "a$" "a\n\n" None;
    test opts "a$" "a\r\n" None;
    test opts {|a\Z|} "a\n" (Some (0, 1));
    test opts {|a\z|} "a\n" None;
    test opts "$" "" (Some (0, 0)));
  test [] "a$" "a\n" (Some (0, 1));
  test [ `Dollar_endonly ] "a$" "a\n" None;
  List.iter
    [ [ `Multiline ]; [ `Multiline; `Dollar_endonly ] ]
    ~f:(fun opts ->
      test opts "a$" "a\nb" (Some (0, 1));
      test opts "a$" "a\n" (Some (0, 1));
      test opts {|a\Z|} "a\nb" None);
  let actual =
    match Re.Pcre.exec ~rex:(Re.Pcre.regexp "a$") "a\n" with
    | exception Not_found -> "no match"
    | groups -> Check.offset (Re.Pcre.get_substring_ofs groups 0)
  in
  Check.check checks {|Re.Pcre "a$" on "a\n"|} ~expected:"(0,1)" actual;
  Check.finish checks;
  [%expect
    {|
    "a$" on "a\n" opts=[]: no match; expected (0,1)
    "a$" on "a\n" opts=[Dollar_endonly]: (0,1); expected no match
    Re.Pcre "a$" on "a\n": no match; expected (0,1)
    23 checks; 3 differences
    |}]
;;
