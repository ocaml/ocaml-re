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

let%expect_test "basic regular expressions are not provided" =
  (* POSIX also defines basic regular expressions, where "\(...\)" groups,
     "\|" alternates and "\{m,n\}" repeats. Re.Posix is an ERE parser, so
     the escaped metacharacters are literals and "\{m,n\}" does not parse. *)
  let show pattern subject =
    Printf.printf
      "%S on %S: %b\n"
      pattern
      subject
      (Re.execp (Posix.compile_pat pattern) subject)
  in
  show {|\(ab\)|} "ab";
  show {|\(ab\)|} "(ab)";
  show {|a\|b|} "a";
  show {|a\|b|} "a|b";
  print_unsupported [ {|a\{2\}|}; {|\(a\)\1|} ];
  [%expect
    {|
    "\\(ab\\)" on "ab": false
    "\\(ab\\)" on "(ab)": true
    "a\\|b" on "a": false
    "a\\|b" on "a|b": true
    "a\\{2\\}": PARSED: (Sequence (Set 97)(Set 123)(Set 50)(Set 125))
    "\\(a\\)\\1": Parse_error
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
  (* POSIX excludes NUL from "." *)
  assert (matches {|.|} "\x00");
  (* [:alpha:] is a fixed byte set that happens to include the lead byte *)
  assert (matches {|[[:alpha:]]|} "\xc3");
  [%expect {||}]
;;
