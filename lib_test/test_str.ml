open Fort_unit
open OUnit2

module type Str_intf = module type of Str

module Test_matches (R : Str_intf) = struct
  let groups () =
    let group i =
      try `Found (R.group_beginning i)
      with
      | Not_found -> `Not_found
      | Invalid_argument _ -> `Not_exists in
    let rec loop acc i =
      match group i with
      | `Found p -> loop ((p, R.group_end i)::acc) (i + 1)
      | `Not_found -> loop ((-1, -1)::acc) (i + 1)
      | `Not_exists -> List.rev acc in
    loop [] 0

  let eq_match ?(pos=0) ?(case=true) r s =
    let pat = if case then R.regexp r else R.regexp_case_fold r in
    try
      ignore (R.search_forward pat s pos);
      Some (groups ())
    with Not_found -> None
end

module T_str = Test_matches(Str)
module T_re = Test_matches(Re_str)

let split_convert = List.map (function
    | Str.Text s -> Re_str.Text s
    | Str.Delim s -> Re_str.Delim s)

let eq_match ?pos ?case r s =
  expect_equal_app
    ~msg:(str_printer s)
    ~printer:(opt_printer (list_printer ofs_printer))
    (fun () -> T_str.eq_match ?pos ?case r s) ()
    (fun () -> T_re.eq_match ?pos ?case r s) ()
;;

let global_replace re s1 s2 =
  let r1 = Re_str.regexp re in
  let r2 = Str.regexp re in
  assert_equal
    ~pp_diff:(fun fmt (expected, actual) ->
        let q fmt s = Format.fprintf fmt "\"%s\"" s in
        let f fmt (name, a1, a2, a3) =
          Format.fprintf fmt "%s %a %a %a" name q a1 q a2 q a3 in
        Format.fprintf fmt "@.%a = %s@.%a = %s@."
          f ("Str.global_replace", re, s1, s2)
          expected
          f ("Re_str.global_replace", re, s1, s2)
          actual)
    ~printer:(fun x -> x)
    (Re_str.global_replace r1 s1 s2)
    (Str.global_replace r2 s1 s2)

let _ =
  (* Literal Match *)
  expect_pass "str" (fun () ->
    eq_match  "a"                 "a";
    eq_match  "a"                 "b";
  );

  (* Basic Operations *)

  expect_pass "alt" (fun () ->
    eq_match  "a\\|b"              "a";
    eq_match  "a\\|b"              "b";
    eq_match  "a\\|b"              "c";
  );

  expect_pass "seq" (fun () ->
    eq_match  "ab"                "ab";
    eq_match  "ab"                "ac";
  );

  expect_pass "epsilon" (fun () ->
    eq_match  ""                  "";
    eq_match  ""                  "a";
  );

  expect_pass "rep" (fun () ->
    eq_match  "a*"                "";
    eq_match  "a*"                "a";
    eq_match  "a*"                "aa";
    eq_match  "a*"                "b";
  );

  expect_pass "rep1" (fun () ->
    eq_match  "a+"                "a";
    eq_match  "a+"                "aa";
    eq_match  "a+"                "";
    eq_match  "a+"                "b";
  );

  expect_pass "opt" (fun () ->
    eq_match  "a?"                "";
    eq_match  "a?"                "a";
  );

  (* String, line, word *)

  expect_pass "bol" (fun () ->
    eq_match  "^a"                "ab";
    eq_match  "^a"                "b\na";
    eq_match  "^a"                "ba";
  );

  expect_pass "eol" (fun () ->
    eq_match  "a$"                "ba";
    eq_match  "a$"                "a\nb";
    eq_match  "a$"                "ba\n";
    eq_match  "a$"                "ab";
  );

  expect_pass "start" (fun () ->
    eq_match ~pos:1 "Za"         "xab";
    eq_match ~pos:1 "Za"         "xb\na";
    eq_match ~pos:1 "Za"         "xba";
  );

  (* Match semantics *)

  expect_pass "match semantics" (fun () ->
    eq_match "\\(a\\|b\\)*b"         "aabaab";
    eq_match "aa\\|aaa"            "aaaa";
    eq_match "aaa\\|aa"            "aaaa";
  );

  (* Group (or submatch) *)

  (* TODO: infinite loop *)
  expect_pass "group" (fun () ->
    eq_match "\\(a\\)\\(a\\)?\\(b\\)"   "ab";
  );

  (* Character set *)

  expect_pass "rg" (fun () ->
    eq_match "[0-9]+"             "0123456789";
    eq_match "[0-9]+"             "a";
  );

  expect_pass "compl" (fun () ->
    eq_match "[^0-9a-z]+"         "A:Z+";
    eq_match "[^0-9a-z]+"         "0";
    eq_match "[^0-9a-z]+"         "a";
  );

  (* Case modifiers *)

  expect_pass "no_case" (fun () ->
    eq_match ~case:false "abc"    "abc";
    eq_match ~case:false "abc"    "ABC";
  );

  expect_pass "global_replace" (fun () ->
    global_replace "needle" "test" "needlehaystack";
    global_replace "needle" "" "";
    global_replace "needle" "" "needle";
    global_replace "xxx" "yyy" "zzz";

    (* the test below fails *)
    (* global_replace "\\(X+\\)" "A\\1YY" "XXXXXXZZZZ" *)
  );

  expect_pass "split tests" (fun () ->
      let printer = list_printer (fun x -> x) in
      let split_printer =
        list_printer (function
            | Re_str.Delim s -> "Delim " ^ s
            | Re_str.Text s -> "Text " ^ s) in
      List.iter (fun (re, s) ->
          let re1 = Str.regexp re in
          let re2 = Re_str.regexp re in
          assert_equal ~printer:split_printer
            (split_convert (Str.full_split re1 s))
            (Re_str.full_split re2 s);
          assert_equal ~printer
            (Str.split_delim re1 s)
            (Re_str.split_delim re2 s);
          assert_equal ~printer
            (Str.split re1 s)
            (Re_str.split re2 s)
        )
        [ "re", ""
        ; " ", "foo bar"
        ; "\b", "one-two three"
        ; "[0-9]", "One3TwoFive"]
    );

  run_test_suite "test_str"
