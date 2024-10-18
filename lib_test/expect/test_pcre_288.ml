open Import
module Pcre = Re_private.Pcre

let whitespace_re = Pcre.regexp "\\s+"

let%expect_test "split1" =
  strings (Pcre.split ~rex:whitespace_re "");
  [%expect {| [] |}]
;;

let%expect_test "split2" =
  strings (Pcre.split ~rex:whitespace_re " ");
  [%expect {|
    [] |}]
;;
