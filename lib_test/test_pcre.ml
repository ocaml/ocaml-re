open OUnit
open Re.Pcre

let sp = Printf.sprintf

let string_of_group = function
  | Text s -> sp "Text %s" s
  | Delim s -> sp "Delim %s" s
  | Group (x, s) -> sp "Group (%d %s)" x s
  | NoGroup -> "NoGroup"
;;

let list_printer f xs = String.concat " ; " (List.map f xs)

let test_blank_class _ =
  let re = Re.Perl.compile_pat "\\d[[:blank:]]\\d[[:blank:]]+[a-z]" in
  let successes = [ "1 2  a"; "2\t3 z"; "9\t0 \t a" ] in
  let failures = [ ""; "123"; "  "; "1 3z" ] in
  List.iter
    (fun s -> assert_bool ("String " ^ s ^ " should match") (Re.execp re s))
    successes;
  List.iter
    (fun s -> assert_bool ("String " ^ s ^ " should not match") (not (Re.execp re s)))
    failures
;;

let rex = regexp "[:_]"
let split_empty _ = assert_equal (full_split ~rex "") []
let split_max_1 _ = assert_equal (full_split ~rex ~max:1 "xxx:yyy") [ Text "xxx:yyy" ]
let rex = regexp "x(x)?"
let printer = list_printer string_of_group

let group_split1 _ =
  let sp = full_split ~rex "testxxyyy" in
  assert_equal ~printer sp [ Text "test"; Delim "xx"; Group (1, "x"); Text "yyy" ]
;;

let group_split2 _ =
  let sp = full_split ~rex "testxyyy" in
  assert_equal ~printer sp [ Text "test"; Delim "x"; NoGroup; Text "yyy" ]
;;

let rex = regexp "(?<many_x>x+)"

let named_groups _ =
  let s = exec ~rex "testxxxyyy" in
  assert_equal (get_named_substring rex "many_x" s) "xxx"
;;

let quote = Printf.sprintf "'%s'"
let pp_str x = x
let pp_list l = l |> List.map quote |> String.concat ", " |> Printf.sprintf "[ %s ]"
let re_whitespace = regexp "[\t ]+"
let re_empty = regexp ""
let re_eol = Re.compile Re.eol
let re_bow = Re.compile Re.bow
let re_eow = Re.compile Re.eow

let test_split () =
  assert_equal
    ~printer:pp_list
    [ "aa"; "bb"; "c"; "d"; "" ]
    (split ~rex:re_whitespace "aa bb c d ");
  assert_equal
    ~printer:pp_list
    [ ""; "a"; "full_word"; "bc"; "" ]
    (split ~rex:re_whitespace " a full_word bc   ");
  assert_equal
    ~printer:pp_list
    [ ""; "a"; "b"; "c"; "d"; "" ]
    (split ~rex:re_empty "abcd");
  assert_equal ~printer:pp_list [ "a"; "\nb"; "" ] (split ~rex:re_eol "a\nb");
  assert_equal ~printer:pp_list [ ""; "a "; "b" ] (split ~rex:re_bow "a b");
  assert_equal ~printer:pp_list [ "a"; " b"; "" ] (split ~rex:re_eow "a b");
  let rex = regexp "" in
  assert_equal ~printer:pp_list (split ~rex "xx") [ ""; "x"; "x"; "" ]
;;

let test_substitute () =
  let rex = regexp "[a-zA-Z]+" in
  let subst = String.capitalize_ascii in
  assert_equal
    ~printer:pp_str
    " Hello World; I Love Chips!"
    (substitute ~rex ~subst " hello world; I love chips!");
  assert_equal ~printer:pp_str "a" (substitute ~rex:re_empty ~subst:(fun _ -> "a") "");
  assert_equal
    ~printer:pp_str
    "*c*t*"
    (substitute ~rex:(regexp "a*") ~subst:(fun _ -> "*") "cat");
  let rex = regexp "^ *" in
  assert_equal ~printer:pp_str (substitute ~rex ~subst:(fun _ -> "A ") "test") "A test"
;;

let test_fixtures =
  "test pcre features"
  >::: [ "test [:blank:] class" >:: test_blank_class
       ; "test splitting empty string" >:: split_empty
       ; "test split with max of 1" >:: split_max_1
       ; "test group split 1" >:: group_split1
       ; "test group split 2 - NoGroup" >:: group_split2
       ; "test named groups" >:: named_groups
       ; "test split" >:: test_split
       ; "test substitute" >:: test_substitute
       ]
;;

let _ = run_test_tt_main test_fixtures
