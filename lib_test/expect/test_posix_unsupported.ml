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

let%expect_test "escaped brackets and braces are not supported" =
  (* POSIX defines "\]" and "\}" as the way to match "]" and "}". They are
     the only ordinary characters whose escape has a defined meaning.
     [Re.Posix] accepts "\{" but rejects these two. *)
  print_unsupported [ {|\]|}; {|\}|}; {|a\]b|}; {|a\}b|} ];
  [%expect
    {|
    "\\]": Parse_error
    "\\}": Parse_error
    "a\\]b": Parse_error
    "a\\}b": Parse_error
    |}]
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

let%expect_test "equivalence classes are silently misparsed" =
  (* POSIX requires "[[=x=]]" to match every collating element equivalent to
     x. [Re.Posix] has no equivalence classes: it parses the bracket as the
     literal characters "[", "=" and "x", and leaves the final "]" as a
     literal, so "[[=a=]]" matches "a]" rather than "a". *)
  let show pattern subject =
    Printf.printf
      "%S on %S: %b\n"
      pattern
      subject
      (Re.execp (Posix.compile_pat pattern) subject)
  in
  show {|[[=a=]]|} "a";
  show {|[[=a=]]|} "a]";
  show {|[[=a=]]|} "=]";
  show {|[[=o=]]|} "o]";
  [%expect
    {|
    "[[=a=]]" on "a": false
    "[[=a=]]" on "a]": true
    "[[=a=]]" on "=]": true
    "[[=o=]]" on "o]": true
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
    "a\\{2\\}": Parse_error
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
