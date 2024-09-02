open! Import
module Fmt = Re_private.Fmt
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
  [%expect
    {|
    33-47, 58-64, 91-96, 123-126, 160-169, 171-180, 182-185, 187-191, 215, 247 |}]
;;

let%expect_test "cany" =
  Format.printf "%a@." Cset.pp Cset.cany;
  [%expect {| 0-255 |}]
;;

let%expect_test "case_insens" =
  let cset = Cset.diff (Cset.case_insens Cset.lower) (Cset.case_insens Cset.upper) in
  Format.printf "%a@." Cset.pp cset;
  [%expect {| 181, 223, 255 |}]
;;

let%expect_test "one_char" =
  let test set =
    let pp fmt c =
      let c = Option.map Cset.to_char c in
      Fmt.(opt char) fmt c
    in
    Format.printf "%a@." pp (Cset.one_char set)
  in
  test Cset.empty;
  [%expect {| <None> |}];
  test (Cset.csingle 'c');
  [%expect {| c |}];
  test Cset.cany;
  [%expect {| <None> |}]
;;

let%expect_test "is_empty" =
  let test set = Format.printf "%a@." Fmt.bool (Cset.is_empty set) in
  test Cset.empty;
  [%expect {| true |}];
  test (Cset.csingle 'c');
  [%expect {| false |}];
  test Cset.cany;
  [%expect {| false |}]
;;

let%expect_test "Cset mem" =
  let test set c = Format.printf "%a@." Fmt.bool (Cset.mem c set) in
  test Cset.cany Cset.null_char;
  [%expect {| false |}];
  test Cset.cany (Cset.of_char 'a');
  [%expect {| true |}];
  let c = Cset.csingle 'c' in
  test c (Cset.of_char 'c');
  [%expect {| true |}];
  test c (Cset.of_char '.');
  [%expect {| false |}]
;;
