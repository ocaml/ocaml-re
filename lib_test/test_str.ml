let eq_match ?pos ?len ?(case = true) r s =
  expect_equal_app
    ~msg:(str_printer s)
    ~printer:arr_ofs_printer
    (fun () ->
      let pos = match pos with None -> 0 | Some p -> p in
      let pat = if case then Str.regexp r else Str.regexp_case_fold r in
      let s_start = Str.search_forward pat s pos in

      (* need a better way to determine group count -
	 maybe parse the regular expression ? *)
      let n_groups =
	try
	  let m = Re.exec ~pos ?len (Re.compile (Re_emacs.re ~case r)) s in
	  Array.length (Re.get_all_ofs m)
	with _ -> 0
      in
      
      (* extract all offset information *)
      let rec get_all_ofs i acc =
	if i >= n_groups then Array.of_list (List.rev acc)
	else
	  let g_begin = try Str.group_beginning i with _ -> -1 in
	  let g_end   = try Str.group_end i with _ -> -1 in
	  get_all_ofs (i + 1) ((g_begin, g_end) :: acc)
      in
      get_all_ofs 0 []
    ) ()
    (fun () ->
      Re.get_all_ofs (
	Re.exec ?pos ?len (Re_emacs.compile (Re_emacs.re ~case r)) s
      )
    ) ()
;;

(* Literal Match *)
expect_pass "str" (fun () ->
  eq_match  "a"                 "a";
  eq_match  "a"                 "b";
);

(* Basic Operations *)

expect_pass "alt" (fun () ->
  eq_match  "a\|b"              "a";
  eq_match  "a\|b"              "b";
  eq_match  "a\|b"              "c";
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

expect_pass "bow" (fun () ->
  eq_match  "\<a"               "a";
  eq_match  "\<a"               "bb aa";
  eq_match  "\<a"               "ba ba";
);

expect_pass "eow" (fun () ->
  eq_match  "\>a"               "a";
  eq_match  "\>a"               "bb aa";
  eq_match  "\>a"               "ab ab";
);

expect_pass "bos" (fun () ->
  eq_match  "\`a"               "ab";
  eq_match  "\`a"               "b\na";
  eq_match  "\`a"               "ba";
);

expect_pass "eos" (fun () ->
  eq_match  "a\'"               "ba";
  eq_match  "a\'"               "a\nb";
  eq_match  "a\'"               "ba\n";
  eq_match  "a\'"               "ab";
);

expect_pass "start" (fun () ->
  eq_match ~pos:1 "\=a"         "xab";
  eq_match ~pos:1 "\=a"         "xb\na";
  eq_match ~pos:1 "\=a"         "xba";
);

expect_pass "not_boundary" (fun () ->
  eq_match "\Bb\B"              "abc";
  eq_match "\Ba"                "abc";
  eq_match "c\B"                "abc";
);

(* Match semantics *)

expect_pass "match semantics" (fun () ->
  eq_match "\(a\|b\)*b"         "aabaab";
  eq_match "aa\|aaa"            "aaaa";
  eq_match "aaa\|aa"            "aaaa";
);

(* Group (or submatch) *)

expect_pass "group" (fun () ->
  eq_match "\(a\)\(a\)?\(b\)"   "ab";
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

