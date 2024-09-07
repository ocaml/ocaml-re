open Import
module Pcre = Re_private.Pcre

let whitespace_re = Pcre.regexp "\\s+"

let strings s =
  Format.printf "[%a]@." Fmt.(list ~pp_sep:(Fmt.lit "; ") Fmt.quoted_string) s
;;

let%expect_test "split1" =
  strings (Pcre.split ~rex:whitespace_re "");
  [%expect {| [""] |}]
;;

let%expect_test "split2" =
  strings (Pcre.split ~rex:whitespace_re " ");
  [%expect {|
    [""; ""] |}]
;;
