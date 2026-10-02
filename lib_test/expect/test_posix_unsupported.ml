open Import
open Re

let outcome pattern =
  match Posix.re_result pattern with
  | Ok re -> Format.asprintf "PARSED: %a" pp re
  | Error `Parse_error -> "Parse_error"
  | Error `Not_supported -> "Not_supported"
;;

let print_unsupported patterns =
  List.iter patterns ~f:(fun pattern ->
    Printf.printf "%S: %s\n" pattern (outcome pattern))
;;

let%expect_test "multi-character collating symbols are not supported" =
  (* POSIX allows a collating element, possibly multi-character, in
     "[[.x.]]". [Re.Posix] accepts a single-character collating symbol and
     treats it as that character, but has no collating order and rejects
     multi-character symbols. *)
  print_unsupported [ {|[[.ch.]]|}; {|[[.hyphen.]]|} ];
  [%expect
    {|
    "[[.ch.]]": Not_supported
    "[[.hyphen.]]": Not_supported
    |}]
;;

let%expect_test "multi-character equivalence classes are not supported" =
  (* POSIX also allows a multi-character collating element in "[=x=]".
     [Re.Posix] handles only the single-character case. *)
  print_unsupported [ {|[[=ab=]]|}; {|[[=ch=]]|} ];
  [%expect
    {|
    "[[=ab=]]": Not_supported
    "[[=ch=]]": Not_supported
    |}]
;;

let%expect_test "basic regular expression backreferences are not supported" =
  (* POSIX basic regular expressions support "\1" through "\9". *)
  let result pattern =
    match Posix.re_result ~opts:[ `Bre ] pattern with
    | Ok _ -> "parsed"
    | Error `Parse_error -> "parse error"
    | Error `Not_supported -> "not supported"
  in
  List.iter [ {|\(a\)\1|}; {|\(a\)\(b\)\2\1|}; {|\1|} ] ~f:(fun pattern ->
    Printf.printf "%S: %s\n" pattern (result pattern));
  [%expect
    {|
    "\\(a\\)\\1": not supported
    "\\(a\\)\\(b\\)\\2\\1": not supported
    "\\1": not supported
    |}]
;;

let%expect_test "matching is byte-oriented, not locale-aware" =
  (* POSIX interprets patterns in the current locale: "." matches one
     (possibly multibyte) character and never NUL, and character classes
     come from the locale's wctype tables. [Re.Posix] works on bytes with
     fixed Latin-1 classes. *)
  let matches pattern subject = Re.execp (Posix.compile_pat pattern) subject in
  (* U+00E9 é is two bytes in UTF-8 *)
  assert (matches {|.|} "\xc3\xa9");
  assert (not (matches {|^.$|} "\xc3\xa9"));
  (* [:alpha:] is a fixed byte set that happens to include the lead byte *)
  assert (matches {|[[:alpha:]]|} "\xc3");
  [%expect {||}]
;;
