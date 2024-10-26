open Import

let print re result =
  Printf.printf
    "%s: %s\n"
    re
    (match result with
     | Ok _ -> "backward range parsed"
     | Error `Parse_error -> "parse error"
     | Error `Not_supported -> "not supported")
;;

let cases = [ "[1-0]"; "[5-1]"; "[6-6]"; "[z-a]"; "[b-b]" ]

let test f =
  List.iter cases ~f:(fun re ->
    let result = f re in
    print re result)
;;

let%expect_test "perl" =
  test Re.Perl.re_result;
  [%expect
    {|
    [1-0]: backward range parsed
    [5-1]: backward range parsed
    [6-6]: backward range parsed
    [z-a]: backward range parsed
    [b-b]: backward range parsed
    |}]
;;

let%expect_test "pcre" =
  test Re.Pcre.re_result;
  [%expect
    {|
    [1-0]: backward range parsed
    [5-1]: backward range parsed
    [6-6]: backward range parsed
    [z-a]: backward range parsed
    [b-b]: backward range parsed
    |}]
;;

let%expect_test "posix" =
  test Re.Posix.re_result;
  [%expect
    {|
    [1-0]: backward range parsed
    [5-1]: backward range parsed
    [6-6]: backward range parsed
    [z-a]: backward range parsed
    [b-b]: backward range parsed
    |}]
;;

(* CR-someday rgrinberg: is this correct? *)
let%expect_test "emacs" =
  test Re.Emacs.re_result;
  [%expect
    {|
    [1-0]: backward range parsed
    [5-1]: backward range parsed
    [6-6]: backward range parsed
    [z-a]: backward range parsed
    [b-b]: backward range parsed
    |}]
;;

(* We allow backward ranges in re. We could forbid them? *)
let%expect_test "re" =
  Format.printf "%a@." Re.pp (Re.rg '5' '0');
  [%expect {| (Set 48-53) |}];
  Format.printf "%a@." Re.pp (Re.rg '0' '5');
  [%expect {| (Set 48-53) |}]
;;
