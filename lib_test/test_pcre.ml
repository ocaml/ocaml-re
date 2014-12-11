open OUnit

let test_blank_class _ =
  let re = Re_perl.compile_pat "\\d[[:blank:]]\\d[[:blank:]]+[a-z]" in
  let successes = ["1 2  a"; "2\t3 z"; "9\t0 \t a"] in
  let failures = [""; "123"; "  "; "1 3z"] in
  successes |> List.iter (fun s ->
    assert_bool ("String " ^ s ^ " should match") (Re.execp re s)
  );
  failures |> List.iter (fun s ->
    assert_bool ("String " ^ s ^ " should not match") (not (Re.execp re s))
  )


let test_fixtures =
  "test pcre features" >:::
  [ "test [:blank:] class" >:: test_blank_class ]

let _ = run_test_tt_main test_fixtures

