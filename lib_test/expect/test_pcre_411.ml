open Import
module Pcre = Re_private.Pcre

let rex =
  Pcre.regexp
    ("^\\s*[v=]*\\s*"
     (* optional version identifier *)
     ^ "([0-9]+|[xX*])(\\.([0-9]+|[xX*])?(\\.([0-9]+|[xX*])?)?)?"
     (* 3-dotted notation *)
     ^ "(?:-((?:[a-zA-Z0-9]+|[a-zA-Z0-9-])(?:\\.[a-zA-Z0-9]+|[a-zA-Z0-9-])*))?"
     ^ (* pre release *)
     "(?:\\+([0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*))?\\s*$" (* build indentifier *))
;;

let sep_re = Pcre.regexp "\\."

let test version =
  try
    let parsed = Pcre.extract ~rex version in
    let pre = Pcre.split ~rex:sep_re parsed.(6) in
    let build = Pcre.split ~rex:sep_re parsed.(7) in
    printf
      "%s %s %s [%s] [%s]\n"
      parsed.(1)
      parsed.(3)
      parsed.(5)
      (String.concat " " pre)
      (String.concat " " build)
  with
  | Not_found -> Printf.printf "%s: Parsing Error. Invalid Version\n" version
;;

let%expect_test "regression test" =
  test "v1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test "=1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test "v 1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test "= 1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test " v1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test " =1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test " v 1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test " = 1.2.3";
  [%expect {| 1 2 3 [] [] |}];
  test "1.2.3-0";
  [%expect {| 1 2 3 [0] [] |}];
  test "v1.2.3-1";
  [%expect {| 1 2 3 [1] [] |}];
  test "v1.2.3-beta";
  [%expect {| 1 2 3 [beta] [] |}];
  test "=1.2.3-beta";
  [%expect {| 1 2 3 [beta] [] |}];
  test "1.2.3-beta";
  [%expect {| 1 2 3 [beta] [] |}];
  test "1.2.3-beta+build";
  [%expect {| 1 2 3 [beta] [build] |}];
  test "1.2.3+build";
  [%expect {| 1 2 3 [] [build] |}];
  test "  v1.2.3+build";
  [%expect {| 1 2 3 [] [build] |}]
;;
