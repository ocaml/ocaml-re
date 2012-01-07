open Re

let re_match ?pos ?len r s res =
  expect_equal_app
    ~msg:(str_printer s)
    ~printer:arr_ofs_printer
    id res
    (fun () -> get_all_ofs (exec ?pos ?len (compile r) s)) ()
;;

let re_fail ?pos ?len r s =
  expect_equal_app
    ~msg:(str_printer s)
    ~printer:arr_ofs_printer
    not_found ()
    (fun () -> get_all_ofs (exec ?pos ?len (compile r) s)) ()
;;

(* Substring Extraction *)

let _ = 
  let r =
     seq [group (char 'a'); 
          opt   (group (char 'a')); 
          group (char 'b')]
  in
  let m = exec (compile r) "ab" in
  
  expect_pass "get" (fun () ->
    expect_eq_str id        "ab" (get m) 0;
    expect_eq_str id        "a"  (get m) 1;
    expect_eq_str not_found ()   (get m) 2;
    expect_eq_str id        "b"  (get m) 3;
    expect_eq_str not_found ()   (get m) 4;
  );

  expect_pass "get_ofs" (fun () ->
    expect_eq_ofs id        (0,2) (get_ofs m) 0;
    expect_eq_ofs id        (0,1) (get_ofs m) 1;
    expect_eq_ofs not_found ()    (get_ofs m) 2;
    expect_eq_ofs id        (1,2) (get_ofs m) 3;
    expect_eq_ofs not_found ()    (get_ofs m) 4;
  );

  expect_pass "get_all" (fun () ->
    expect_eq_arr_str
      id [|"ab";"a";"";"b"|]
      get_all m
  );

  expect_pass "get_all_ofs" (fun () ->
    expect_eq_arr_ofs
      id [|(0,2);(0,1);(-1,-1);(1,2)|]
      get_all_ofs m
  );

  expect_pass "test" (fun () ->
    expect_eq_bool id true (test m) 0;
    expect_eq_bool id true (test m) 1;
    expect_eq_bool id false (test m) 2;
    expect_eq_bool id true (test m) 3;
    expect_eq_bool id false (test m) 4;
  );
;;

(* Literal Match *)

expect_pass "str" (fun () ->
  re_match  (str "a")                   "a"     [|(0,1)|];
  re_fail   (str "a")                   "b";
);

expect_pass "char" (fun () ->
  re_match  (char 'a')                  "a"     [|(0,1)|];
  re_fail   (char 'a')                  "b";
);

(* Basic Operations *)

expect_pass "alt" (fun () ->
  re_match  (alt [char 'a'; char 'b'])  "a"     [|(0,1)|];
  re_match  (alt [char 'a'; char 'b'])  "b"     [|(0,1)|];
  re_fail   (alt [char 'a'; char 'b'])  "c";
);

expect_pass "seq" (fun () ->
  re_match  (seq [char 'a'; char 'b'])  "ab"    [|(0,2)|];
  re_fail   (seq [char 'a'; char 'b'])  "ac";
);

expect_pass "empty" (fun () ->
  re_fail   (empty)                     "";
  re_fail   (empty)                     "a";
);

expect_pass "epsilon" (fun () ->
  re_match  (epsilon)                   ""      [|(0,0)|];
  re_match  (epsilon)                   "a"     [|(0,0)|];
);

expect_pass "rep" (fun () ->
  re_match  (rep (char 'a'))            ""      [|(0,0)|];
  re_match  (rep (char 'a'))            "a"     [|(0,1)|];
  re_match  (rep (char 'a'))            "aa"    [|(0,2)|];
  re_match  (rep (char 'a'))            "b"     [|(0,0)|];
);

expect_pass "rep1" (fun () ->
  re_match  (rep1 (char 'a'))           "a"     [|(0,1)|];
  re_match  (rep1 (char 'a'))           "aa"    [|(0,2)|];
  re_fail   (rep1 (char 'a'))           "";
  re_fail   (rep1 (char 'a'))           "b";
);

expect_pass "repn" (fun () ->
  re_match  (repn (char 'a') 0 None)            ""      [|(0,0)|];
  re_match  (repn (char 'a') 0 (Some 0))        ""      [|(0,0)|];
  re_match  (repn (char 'a') 1 (Some 2))        "a"     [|(0,1)|];
  re_match  (repn (char 'a') 1 (Some 2))        "aa"    [|(0,2)|];
  re_fail   (repn (char 'a') 1 (Some 2))        "";
  re_match  (repn (char 'a') 1 (Some 2))        "aaa"   [|(0,2)|];

  expect_equal_app
    invalid_arg "Re.repn"
    (fun () -> repn empty (-1) None)  ();

  expect_equal_app
    invalid_arg "Re.repn"
    (fun () -> repn empty 1 (Some 0)) ();
);

expect_pass "opt" (fun () ->
  re_match  (opt (char 'a'))            ""      [|(0,0)|];
  re_match  (opt (char 'a'))            "a"     [|(0,1)|];
);

(* String, line, word *)

expect_pass "bol" (fun () ->
  re_match  (seq [bol; char 'a'])       "ab"    [|(0,1)|];
  re_match  (seq [bol; char 'a'])       "b\na"  [|(2,3)|];
  re_fail   (seq [bol; char 'a'])       "ba";
);

expect_pass "eol" (fun () ->
  re_match  (seq [char 'a'; eol])       "ba"    [|(1,2)|];
  re_match  (seq [char 'a'; eol])       "a\nb"  [|(0,1)|];
  re_match  (seq [char 'a'; eol])       "ba\n"  [|(1,2)|];
  re_fail   (seq [char 'a'; eol])       "ab";
);

expect_pass "bow" (fun () ->
  re_match  (seq [bow; char 'a'])       "a"     [|(0,1)|];
  re_match  (seq [bow; char 'a'])       "bb aa" [|(3,4)|];
  re_fail   (seq [bow; char 'a'])       "ba ba";
);

expect_pass "eow" (fun () ->
  re_match  (seq [char 'a'; eow])       "a"     [|(0,1)|];
  re_match  (seq [char 'a'; eow])       "bb aa" [|(4,5)|];
  re_fail   (seq [char 'a'; eow])       "ab ab";
);

expect_pass "bos" (fun () ->
  re_match  (seq [bos; char 'a'])       "ab"    [|(0,1)|];
  re_fail   (seq [bos; char 'a'])       "b\na";
  re_fail   (seq [bos; char 'a'])       "ba";
);

expect_pass "eos" (fun () ->
  re_match  (seq [char 'a'; eos])       "ba"    [|(1,2)|];
  re_fail   (seq [char 'a'; eos])       "a\nb";
  re_fail   (seq [char 'a'; eos])       "ba\n";
  re_fail   (seq [char 'a'; eos])       "ab";
);

expect_pass "leol" (fun () ->
  re_match  (seq [char 'a'; leol])      "ba"    [|(1,2)|];
  re_fail   (seq [char 'a'; leol])      "a\nb";
  re_match  (seq [char 'a'; leol])      "ba\n"  [|(1,2)|];
  re_fail   (seq [char 'a'; leol])      "ab";
  re_match  (alt [str "b\n"; seq [char 'a'; leol]])  "ab\n"    [|(1,3)|];
);

expect_pass "start" (fun () ->
  re_match ~pos:1 (seq [start; char 'a'])      "xab"    [|(1,2)|];
  re_fail  ~pos:1 (seq [start; char 'a'])      "xb\na";
  re_fail  ~pos:1 (seq [start; char 'a'])      "xba";
);

expect_pass "stop" (fun () ->
  re_match ~len:2 (seq [char 'a'; stop])       "bax"    [|(1,2)|];
  re_fail  ~len:3 (seq [char 'a'; stop])       "a\nbx";
  re_fail  ~len:3 (seq [char 'a'; stop])       "ba\nx";
  re_fail  ~len:2 (seq [char 'a'; stop])       "abx";
);

expect_pass "word" (fun () ->
  re_match  (word (str "aa"))           "aa"    [|(0,2)|];
  re_match  (word (str "aa"))           "bb aa" [|(3,5)|];
  re_fail   (word (str "aa"))           "aaa";
);

expect_pass "not_boundary" (fun () ->
  re_match (seq [not_boundary; char 'b'; not_boundary])  "abc"  [|(1,2)|];
  re_fail  (seq [not_boundary; char 'a'])  "abc";
  re_fail  (seq [char 'c'; not_boundary])  "abc";
);

(* Match semantics *)

expect_pass "default match semantics" (fun () ->
  re_match
    (seq [(rep (alt [char 'a'; char 'b'])); char 'b'])
    "aabaab"
    [|(0,6)|];
  re_match
    (alt [str "aa"; str "aaa"])
    "aaaa"
    [|(0, 2)|];
  re_match
    (alt [str "aaa"; str "aa"])
    "aaaa"
    [|(0, 3)|];
);

expect_pass "shortest match" (fun () ->
  re_match
    (shortest (seq [(rep (alt [char 'a'; char 'b'])); char 'b']))
    "aabaab"
    [|(0,3)|];
  re_match
    (shortest (alt [str "aa"; str "aaa"]))
    "aaaa"
    [|(0, 2)|];
  re_match
    (shortest (alt [str "aaa"; str "aa"]))
    "aaaa"
    [|(0, 2)|];
);

expect_pass "longest match" (fun () ->
  re_match
    (longest (seq [(rep (alt [char 'a'; char 'b'])); char 'b']))
    "aabaab"
    [|(0,6)|];
  re_match
    (longest (alt [str "aa"; str "aaa"]))
    "aaaa"
    [|(0, 3)|];
  re_match
    (longest (alt [str "aaa"; str "aa"]))
    "aaaa"
    [|(0, 3)|];
);

expect_pass "first match" (fun () ->
  re_match
    (first (seq [(rep (alt [char 'a'; char 'b'])); char 'b']))
    "aabaab"
    [|(0,6)|];
  re_match
    (first (alt [str "aa"; str "aaa"]))
    "aaaa"
    [|(0, 2)|];
  re_match
    (first (alt [str "aaa"; str "aa"]))
    "aaaa"
    [|(0, 3)|];
);

expect_pass "greedy" (fun () ->
  re_match
    (greedy (seq [(rep (alt [char 'a'; char 'b'])); char 'b']))
    "aabaab"
    [|(0,6)|];
  re_match
    (greedy (rep (group (opt (char 'a')))))
    "aa"
    [|(0,2); (2,2)|];
);

expect_pass "non_greedy" (fun () ->
  re_match
    (non_greedy (longest (seq [(rep (alt [char 'a'; char 'b'])); char 'b'])))
    "aabaab"
    [|(0,6)|];
  re_match
    (non_greedy (first (seq [(rep (alt [char 'a'; char 'b'])); char 'b'])))
    "aabaab"
    [|(0,3)|];
  re_match
    (non_greedy (longest (rep (group (opt (char 'a'))))))
    "aa"
    [|(0,2); (1,2)|];
);

expect_pass "match semantics" (fun () ->
  let r = rep (group (alt [str "aaa"; str "aa"])) in
  re_match (longest r)            "aaaaaaa" [|(0,7); (5, 7)|];
  re_match (first r)              "aaaaaaa" [|(0,6); (3, 6)|];
  re_match (first (non_greedy r)) "aaaaaaa" [|(0,0); (-1, -1)|];
  re_match (shortest r)           "aaaaaaa" [|(0,0); (-1, -1)|];
  let r' = rep (group (shortest (alt [str "aaa"; str "aa"]))) in
  re_match (longest r')           "aaaaaaa" [|(0,7); (4, 7)|];
  re_match (first r')             "aaaaaaa" [|(0,6); (4, 6)|];
);

(* Group (or submatch) *)

expect_pass "group" (fun () ->
  let r =
     seq [group (char 'a'); 
          opt   (group (char 'a')); 
          group (char 'b')]
  in
  expect_eq_arr_ofs
    id [|(0,2);(0,1);(-1,-1);(1,2)|]
    (fun () -> get_all_ofs (exec (compile r) "ab")) ()
);

expect_pass "no_group" (fun () ->
  let r =
     no_group (
       seq [group (char 'a'); 
            opt   (group (char 'a')); 
            group (char 'b')]
     )
  in
  expect_eq_arr_ofs
    id [|(0,2)|]
    (fun () -> get_all_ofs (exec (compile r) "ab")) ()
);

expect_pass "nest" (fun () ->
  let r =
    rep (nest (alt [group (char 'a'); char 'b']))
  in
  re_match r "ab" [|(0,2); (-1, -1)|];
  re_match r "ba" [|(0,2); (1, 2)|];
);

(* Character set *)

expect_pass "set" (fun () ->
  re_match (rep1 (set "abcd")) "bcbadbabcdba" [|(0,12)|];
  re_fail  (set "abcd") "e";
);

expect_pass "rg" (fun () ->
  re_match (rep1 (rg '0' '9')) "0123456789" [|(0,10)|];
  re_fail  (rep1 (rg '0' '9')) "a";
);

expect_pass "inter" (fun () ->
  re_match (rep1 (inter [rg '0' '9'; rg '4' '6']))  "456"   [|(0,3)|];
  re_fail  (rep1 (inter [rg '0' '9'; rg '4' '6']))  "7";
  re_match (inter [alt [char 'a'; char 'b']; char 'b']) "b" [|(0,1)|];
);

expect_pass "diff" (fun () ->
  re_match (rep1 (diff (rg '0' '9') (rg '4' '6')))  "0123789"   [|(0,7)|];
  re_fail  (rep1 (diff (rg '0' '9') (rg '4' '6')))  "4";
);

expect_pass "compl" (fun () ->
  re_match (rep1 (compl [rg '0' '9'; rg 'a' 'z'])) "A:Z+" [|(0,4)|];
  re_fail  (rep1 (compl [rg '0' '9'; rg 'a' 'z'])) "0";
  re_fail  (rep1 (compl [rg '0' '9'; rg 'a' 'z'])) "a";
);

(* Predefined character sets - should these be tested exhaustively? *)

(* Case modifiers *)

expect_pass "case" (fun () ->
  re_match (case (str "abc"))           "abc" [|(0,3)|];
  re_match (no_case (case (str "abc"))) "abc" [|(0,3)|];
  re_fail  (case (str "abc"))           "ABC";
  re_fail  (no_case (case (str "abc"))) "ABC";
);

expect_pass "no_case" (fun () ->
  re_match (no_case (str "abc"))        "abc" [|(0,3)|];
  re_match (no_case (str "abc"))        "ABC" [|(0,3)|];
  re_match (case (no_case (str "abc"))) "abc" [|(0,3)|];
  re_match (case (no_case (str "abc"))) "ABC" [|(0,3)|];
);

(* Fixed bugs *)

expect_pass "bugs" (fun () ->
  try
    ignore (Re.compile (Re_perl.re "(.*?)(\\WPl|\\Bpl)(.*)"))
  with _ ->
    fail "bug in Re.handle_case"
);
