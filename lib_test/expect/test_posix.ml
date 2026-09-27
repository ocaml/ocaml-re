open Import

let%expect_test "class space" =
  let re = Re.Posix.compile_pat {|a[[:space:]]b|} in
  let exec = Re.execp re in
  assert (exec "a b");
  assert (not (exec "ab"));
  assert (not (exec "a_b"));
  [%expect {||}]
;;

let outcome pattern subject =
  match Re.Posix.re_result pattern with
  | Error `Parse_error -> "parse error"
  | Error `Not_supported -> "not supported"
  | Ok re ->
    (match Re.exec_opt (Re.compile re) subject with
     | None -> "no match"
     | Some groups -> Printf.sprintf "match %S" (Re.Group.get groups 0))
;;

let show pattern subject =
  Printf.printf "%S on %S: %s\n" pattern subject (outcome pattern subject)
;;

let%expect_test "unsupported constructs" =
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
    "\\]" on "]": parse error
    "\\}" on "}": parse error
    "^[[=a=]]$" on "a": no match
    "^[[=a=]]$" on "a]": match "a]"
    "^[a[=b=]]$" on "b]": match "b]"
    |}]
;;
