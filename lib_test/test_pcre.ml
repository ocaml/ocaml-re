open OUnit
open Re_pcre

let test_blank_class _ =
  let re = Re_perl.compile_pat {|\d[[:blank:]]\d[[:blank:]]+[a-z]|} in
  let successes = ["1 2  a"; "2\t3 z"; "9\t0 \t a"] in
  let failures = [""; "123"; "  "; "1 3z"] in
  successes |> List.iter (fun s ->
    assert_bool ("String " ^ s ^ " should match") (Re.execp re s)
  );
  failures |> List.iter (fun s ->
    assert_bool ("String " ^ s ^ " should not match") (not @@ Re.execp re s)
  )

let rex = regexp "[:_]"

let split_empty _ =
  assert_equal (full_split ~rex "") []

let split_max_1 _ =
  assert_equal (full_split ~rex ~max:1 "xxx:yyy") [Text "xxx:yyy"]

let rex = regexp "x(x)?"
let group_split1 _ =
  let sp = full_split ~rex "testxxyyy" in
  assert_equal sp [Text "test"; Delim "xx"; Group (1, "x"); Text "yyy"]

let group_split2 _ =
  let sp = full_split ~rex "testxyyy" in
  assert_equal sp [Text "test"; Delim "xx"; NoGroup; Text "yyy"]

let test_fixtures =
  "test pcre features" >:::
  [ "test [:blank:] class" >:: test_blank_class
  ; "test splitting empty string" >:: split_empty
  ; "test split with max of 1" >:: split_max_1
  ; "test group split 1" >:: group_split1
  ; "test group split 2 - NoGroup" >:: group_split2]

let _ = run_test_tt_main test_fixtures

