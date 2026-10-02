open Import

let%expect_test "class space" =
  let re = Re.Posix.compile_pat {|a[[:space:]]b|} in
  let exec = Re.execp re in
  assert (exec "a b");
  assert (not (exec "ab"));
  assert (not (exec "a_b"));
  [%expect {||}]
;;

let outcome ?(opts = []) pattern subject =
  match Re.Posix.re_result ~opts pattern with
  | Error `Parse_error -> "parse error"
  | Error `Not_supported -> "not supported"
  | Ok re ->
    (match Re.exec_opt (Re.compile re) subject with
     | None -> "no match"
     | Some groups -> Printf.sprintf "match %S" (Re.Group.get groups 0))
;;

let show ?opts pattern subject =
  Printf.printf "%S on %S: %s\n" pattern subject (outcome ?opts pattern subject)
;;

let%expect_test "escaped brackets and equivalence classes" =
  List.iter
    [ {|\]|}, "]"
    ; {|\}|}, "}"
    ; {|^[[=a=]]$|}, "a"
    ; {|^[[=a=]]$|}, "a]"
    ; {|^[a[=b=]]$|}, "b]"
    ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "\\]" on "]": match "]"
    "\\}" on "}": match "}"
    "^[[=a=]]$" on "a": match "a"
    "^[[=a=]]$" on "a]": no match
    "^[a[=b=]]$" on "b]": no match
    |}]
;;

let%expect_test "dot and NUL" =
  List.iter
    [ {|.|}, "\x00"; {|.|}, "a" ]
    ~f:(fun (pattern, subject) -> show pattern subject);
  [%expect
    {|
    "." on "\000": no match
    "." on "a": match "a"
    |}]
;;

let%expect_test "dot and NUL with Newline" =
  let matches pattern subject =
    Re.execp (Re.Posix.compile_pat ~opts:[ `Newline ] pattern) subject
  in
  Printf.printf "NUL: %b\n" (matches {|.|} "\x00");
  Printf.printf "newline: %b\n" (matches {|.|} "\n");
  [%expect
    {|
    NUL: false
    newline: false
    |}]
;;

let%expect_test "basic regular expressions" =
  List.iter
    [ {|^\(ab\)$|}, "ab"
    ; {|^\(ab\)$|}, "(ab)"
    ; {|\(a\|b\)c|}, "ac"
    ; {|\(a\|b\)c|}, "bc"
    ; {|^a\{2\}$|}, "aa"
    ; {|^a\{2\}$|}, "a"
    ; {|^a\{2,\}$|}, "aaa"
    ; {|^a\{2,3\}$|}, "aaa"
    ; {|^a+$|}, "a"
    ; {|^a+$|}, "a+"
    ; {|^a?$|}, "a"
    ; {|^a?$|}, "a?"
    ; {|^a{2}$|}, "aa"
    ; {|^a{2}$|}, "a{2}"
    ; {|^(ab)$|}, "(ab)"
    ; {|^(ab)$|}, "ab"
    ; {|a$b|}, "a$b"
    ; {|a$b|}, "ab"
    ; {|a$|}, "a"
    ; {|^a|}, "a"
    ; {|^a|}, "ba"
    ; {|*a|}, "*a"
    ; {|a\{,2\}|}, "a"
    ]
    ~f:(fun (pattern, subject) -> show ~opts:[ `Bre ] pattern subject);
  [%expect
    {|
    "^\\(ab\\)$" on "ab": match "ab"
    "^\\(ab\\)$" on "(ab)": no match
    "\\(a\\|b\\)c" on "ac": match "ac"
    "\\(a\\|b\\)c" on "bc": match "bc"
    "^a\\{2\\}$" on "aa": match "aa"
    "^a\\{2\\}$" on "a": no match
    "^a\\{2,\\}$" on "aaa": match "aaa"
    "^a\\{2,3\\}$" on "aaa": match "aaa"
    "^a+$" on "a": no match
    "^a+$" on "a+": match "a+"
    "^a?$" on "a": no match
    "^a?$" on "a?": match "a?"
    "^a{2}$" on "aa": no match
    "^a{2}$" on "a{2}": match "a{2}"
    "^(ab)$" on "(ab)": match "(ab)"
    "^(ab)$" on "ab": no match
    "a$b" on "a$b": match "a$b"
    "a$b" on "ab": no match
    "a$" on "a": match "a"
    "^a" on "a": match "a"
    "^a" on "ba": no match
    "*a" on "*a": match "*a"
    "a\\{,2\\}" on "a": parse error
    |}]
;;

let%expect_test "basic regular expression groups capture" =
  let re = Re.Posix.compile_pat ~opts:[ `Bre ] {|\(a\)\(b\)|} in
  Array.iter (Printf.printf "%S\n") (Re.Group.all (Re.exec re "ab"));
  [%expect
    {|
    "ab"
    "a"
    "b"
    |}]
;;
