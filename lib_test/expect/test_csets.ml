open! Import
module Cset = Re_private.Cset

let%expect_test "empty" =
  Format.printf "%a@." Cset.pp Cset.empty;
  [%expect {| |}]
;;

let%expect_test "ascii" =
  Format.printf "%a@." Cset.pp Cset.ascii;
  [%expect {| 0-127 |}]
;;

let%expect_test "cdigit" =
  Format.printf "%a@." Cset.pp Cset.cdigit;
  [%expect {| 48-57 |}]
;;

let%expect_test "calpha" =
  Format.printf "%a@." Cset.pp Cset.calpha;
  [%expect {|
    65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255 |}]
;;

let%expect_test "cword" =
  Format.printf "%a@." Cset.pp Cset.cword;
  [%expect {|
    48-57, 65-90, 95, 97-122, 170, 181, 186, 192-214, 216-246, 248-255 |}]
;;

let%expect_test "notnl" =
  Format.printf "%a@." Cset.pp Cset.notnl;
  [%expect {|
    0-9, 11-255 |}]
;;

let%expect_test "nl" =
  Format.printf "%a@." Cset.pp Cset.nl;
  [%expect {| 10 |}]
;;

let%expect_test "blank" =
  Format.printf "%a@." Cset.pp Cset.nl;
  [%expect {| 10 |}]
;;

let%expect_test "space" =
  Format.printf "%a@." Cset.pp Cset.space;
  [%expect {|
    9-13, 32 |}]
;;

let%expect_test "xdigit" =
  Format.printf "%a@." Cset.pp Cset.xdigit;
  [%expect {|
    48-57, 65-70, 97-102 |}]
;;

let%expect_test "lower" =
  Format.printf "%a@." Cset.pp Cset.lower;
  [%expect {|
    97-122, 181, 223-246, 248-255 |}]
;;

let%expect_test "upper" =
  Format.printf "%a@." Cset.pp Cset.upper;
  [%expect {|
    65-90, 192-214, 216-222 |}]
;;

let%expect_test "alpha" =
  Format.printf "%a@." Cset.pp Cset.alpha;
  [%expect {|
    65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255 |}]
;;

let%expect_test "alnum" =
  Format.printf "%a@." Cset.pp Cset.alnum;
  [%expect {|
    48-57, 65-90, 97-122, 170, 181, 186, 192-214, 216-246, 248-255 |}]
;;

let%expect_test "wordc" =
  Format.printf "%a@." Cset.pp Cset.wordc;
  [%expect {|
    48-57, 65-90, 95, 97-122, 170, 181, 186, 192-214, 216-246, 248-255 |}]
;;

let%expect_test "cntrl" =
  Format.printf "%a@." Cset.pp Cset.cntrl;
  [%expect {|
    0-31, 127-159 |}]
;;

let%expect_test "graph" =
  Format.printf "%a@." Cset.pp Cset.graph;
  [%expect {|
    33-126, 160-255 |}]
;;

let%expect_test "print" =
  Format.printf "%a@." Cset.pp Cset.print;
  [%expect {|
    32-126, 160-255 |}]
;;

let%expect_test "punct" =
  Format.printf "%a@." Cset.pp Cset.punct;
  [%expect {|
    33-47, 58-64, 91-96, 123-126, 160-169, 171-180, 182-185, 187-191, 215, 247 |}]
;;

let%expect_test "cany" =
  Format.printf "%a@." Cset.pp Cset.cany;
  [%expect {| 0-255 |}]
;;
