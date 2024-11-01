open Import

module type Str_intf = module type of Str

module Test_matches (R : Str_intf) = struct
  let groups () =
    let group i =
      try `Found (R.group_beginning i) with
      | Not_found -> `Not_found
      | Invalid_argument _ -> `Not_exists
    in
    let rec loop acc i =
      match group i with
      | `Found p -> loop ((p, R.group_end i) :: acc) (i + 1)
      | `Not_found -> loop ((-1, -1) :: acc) (i + 1)
      | `Not_exists -> List.rev acc
    in
    loop [] 0
  ;;

  let eq_match ?(pos = 0) ?(case = true) r s =
    let pat = if case then R.regexp r else R.regexp_case_fold r in
    try
      ignore (R.search_forward pat s pos);
      Some (groups ())
    with
    | Not_found -> None
  ;;

  let eq_match' ?(pos = 0) ?(case = true) r s =
    let pat = if case then R.regexp r else R.regexp_case_fold r in
    try
      ignore (R.string_match pat s pos);
      Some (groups ())
    with
    | Not_found -> None
  ;;
end

module T_str = Test_matches (Str)
module T_re = Test_matches (Re.Str)

let test dyn_of_ok str re args =
  let run f =
    match f () with
    | s -> Ok s
    | exception exn -> Error exn
  in
  let str = run (fun () -> str args) in
  let re = run (fun () -> re args) in
  if not (Poly.equal str re)
  then (
    let printer x =
      let dyn =
        let open Dyn in
        result dyn_of_ok (fun x -> string (Printexc.to_string x)) x
      in
      sexp_of_dyn dyn |> Base.Sexp.to_string_hum
    in
    Printf.printf "str: %s\n" (printer str);
    Printf.printf "re: %s\n" (printer re))
;;

let dyn_of_pairs x =
  Dyn.option
    (fun x ->
      List.map x ~f:(fun (start, stop) ->
        let open Dyn in
        pair (int start) (int stop))
      |> Dyn.list)
    x
;;

let split_result_conv =
  List.map ~f:(function
    | Str.Delim x -> Re.Str.Delim x
    | Str.Text x -> Re.Str.Text x)
;;

let dyn_split_result_list list =
  List.map
    list
    ~f:
      (let open Dyn in
       function
       | Re.Str.Delim x -> variant "Delim" [ string x ]
       | Text s -> variant "Text" [ string s ])
  |> Dyn.list
;;

type ('a, 'b) test =
  { name : string
  ; dyn_of_ok : 'b -> Dyn.t
  ; re_str : Re.Str.regexp -> 'a -> 'b
  ; str : Str.regexp -> 'a -> 'b
  }

let bounded_split_t =
  { name = "bounded_split"
  ; dyn_of_ok = (fun x -> Dyn.list (List.map x ~f:Dyn.string))
  ; re_str = (fun re (s, n) -> Re.Str.bounded_split re s n)
  ; str = (fun re (s, n) -> Str.bounded_split re s n)
  }
;;

let bounded_full_split_t =
  { name = "bounded_full_split"
  ; dyn_of_ok = dyn_split_result_list
  ; re_str = (fun re (s, n) -> Re.Str.bounded_full_split re s n)
  ; str = (fun re (s, n) -> split_result_conv (Str.bounded_full_split re s n))
  }
;;

let full_split_t =
  { bounded_full_split_t with
    name = "full_split"
  ; re_str = (fun re s -> Re.Str.full_split re s)
  ; str = (fun re s -> split_result_conv (Str.full_split re s))
  }
;;

let split_delim_t =
  { name = "split_delim"
  ; dyn_of_ok = (fun x -> Dyn.list (List.map x ~f:Dyn.string))
  ; re_str = Re.Str.split_delim
  ; str = Str.split_delim
  }
;;

let split_t =
  { name = "split"
  ; dyn_of_ok = (fun x -> Dyn.list (List.map x ~f:Dyn.string))
  ; re_str = Re.Str.split
  ; str = Str.split
  }
;;

let global_replace_t =
  { name = "global_replace"
  ; dyn_of_ok = Dyn.string
  ; re_str = (fun re (r, s) -> Re.Str.global_replace re r s)
  ; str = (fun re (r, s) -> Str.global_replace re r s)
  }
;;

let eq_match ?pos ?case re =
  test dyn_of_pairs (T_str.eq_match ?pos ?case re) (T_re.eq_match ?pos ?case re)
;;

let eq_match' ?pos ?case re =
  test dyn_of_pairs (T_str.eq_match' ?pos ?case re) (T_re.eq_match' ?pos ?case re)
;;

let test t re args =
  test t.dyn_of_ok (t.re_str (Re.Str.regexp re)) (t.str (Str.regexp re)) args
;;

let split_delim re s = test split_delim_t re s
let split re s = test split_t re s
let full_split re s = test full_split_t re s
let bounded_split re s n = test bounded_split_t re (s, n)
let bounded_full_split re s n = test bounded_full_split_t re (s, n)
let global_replace re r s = test global_replace_t re (r, s)

let%expect_test "literal match" =
  eq_match "a" "a";
  eq_match "a" "b";
  [%expect {||}]
;;

let%expect_test "alt" =
  eq_match "a\\|b" "a";
  eq_match "a\\|b" "b";
  eq_match "a\\|b" "c";
  [%expect {||}]
;;

let%expect_test "seq" =
  eq_match "ab" "ab";
  eq_match "ab" "ac";
  [%expect {||}]
;;

let%expect_test "epsilon" =
  eq_match "" "";
  eq_match "" "a";
  [%expect {||}]
;;

let%expect_test "rep" =
  eq_match "a*" "";
  eq_match "a*" "a";
  eq_match "a*" "aa";
  eq_match "a*" "b";
  [%expect {||}]
;;

let%expect_test "rep1" =
  eq_match "a+" "a";
  eq_match "a+" "aa";
  eq_match "a+" "";
  eq_match "a+" "b";
  [%expect {| |}]
;;

let%expect_test "opt" =
  eq_match "a?" "";
  eq_match "a?" "a";
  [%expect {| |}]
;;

let%expect_test "bol" =
  eq_match "^a" "ab";
  eq_match "^a" "b\na";
  eq_match "^a" "ba";
  [%expect {| |}]
;;

let%expect_test "eol" =
  eq_match "a$" "ba";
  eq_match "a$" "a\nb";
  eq_match "a$" "ba\n";
  eq_match "a$" "ab";
  [%expect {| |}]
;;

let%expect_test "start" =
  eq_match ~pos:1 "Za" "xab";
  eq_match ~pos:1 "Za" "xb\na";
  eq_match ~pos:1 "Za" "xba";
  [%expect {||}]
;;

let%expect_test "match semantics" =
  eq_match "\\(a\\|b\\)*b" "aabaab";
  eq_match "aa\\|aaa" "aaaa";
  eq_match "aaa\\|aa" "aaaa";
  [%expect {||}]
;;

let%expect_test "Group (or submatch)" =
  eq_match "\\(a\\)\\(a\\)?\\(b\\)" "ab";
  [%expect {| |}];
  eq_match "\\(foo" "foo";
  [%expect {|
    str: (Error "Failure(\"\\\\( group not closed by \\\\)\")")
    re: (Error Re_private.Emacs.Parse_error)
    |}]
;;

let%expect_test "Character set" =
  eq_match "[0-9]+" "0123456789";
  eq_match "[0-9]+" "a";
  eq_match "[9-0]+" "2";
  eq_match "[5-5]" "5";
  eq_match "[5-4]" "1";
  eq_match' "[]]" "]";
  eq_match' "[a-]" "-";
  eq_match' "[-a]" "-";
  eq_match' "]" "]";
  eq_match' "[^b-f]" "z";
  eq_match' "[^b-f]" "a";
  [%expect {||}];
  (* These errors aren't correct *)
  eq_match' "[]" "x";
  eq_match' "[" "[";
  [%expect
    {|
    str: (Error "Failure(\"[ class not closed by ]\")")
    re: (Error Re_private.Emacs.Parse_error)
    str: (Error "Failure(\"[ class not closed by ]\")")
    re: (Error Re_private.Emacs.Parse_error)
    |}]
;;

let%expect_test "compl" =
  eq_match "[^0-9a-z]+" "A:Z+";
  eq_match "[^0-9a-z]+" "0";
  eq_match "[^0-9a-z]+" "a";
  [%expect {||}]
;;

let%expect_test "Word modifiers" =
  eq_match' "\\bfoo" "foo";
  eq_match' "\\<foo" "foo";
  eq_match' "foo\\>" "foo";
  eq_match' "z\\Bfoo" "zfoo";
  eq_match' "\\`foo" "foo";
  eq_match' "foo\\'" "foo";
  [%expect {||}]
;;

let%expect_test "Case modifiers" =
  eq_match ~case:false "abc" "abc";
  eq_match ~case:false "abc" "ABC";
  [%expect {| |}]
;;

let%expect_test "global_replace" =
  global_replace "needle" "test" "needlehaystack";
  global_replace "needle" "" "";
  global_replace "needle" "" "needle";
  global_replace "xxx" "yyy" "zzz";
  global_replace "test\\([0-9]*\\)" "\\1-foo-\\1" "test100 test200 test";
  global_replace "test\\([0-9]*\\)" "'\\-0'" "test100 test200 test";
  (* Regrssion test for #129 *)
  global_replace "\\(X+\\)" "A\\1YY" "XXXXXXZZZZ";
  [%expect {||}]
;;

let%expect_test "bounded_split, bounded_full_split" =
  [ ",", "foo,bar,baz", 5
  ; ",", "foo,bar,baz", 1
  ; ",", "foo,bar,baz", 0
  ; ",\\|", "foo,bar|baz", 4
  ]
  |> List.iter ~f:(fun (re, s, n) ->
    bounded_full_split re s n;
    bounded_split re s n);
  [%expect {||}]
;;

let%expect_test "split, full_split, split_delim" =
  [ "re", ""; " ", "foo bar"; "\b", "one-two three"; "[0-9]", "One3TwoFive" ]
  |> List.iter ~f:(fun (re, s) ->
    split re s;
    full_split re s;
    split_delim re s);
  [%expect {||}]
;;
