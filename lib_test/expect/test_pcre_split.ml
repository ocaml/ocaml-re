open Import

let split ~rex s = Re.Pcre.split ~rex s |> strings

let%expect_test "split" =
  split ~rex:re_whitespace "aa bb c d ";
  [%expect {| ["aa"; "bb"; "c"; "d"] |}];
  split ~rex:re_whitespace " a full_word bc   ";
  [%expect {| ["a"; "full_word"; "bc"] |}];
  split ~rex:re_empty "abcd";
  [%expect {| ["a"; "b"; "c"; "d"] |}];
  split ~rex:re_eol "a\nb";
  [%expect {| ["a"; "\nb"] |}];
  split ~rex:re_bow "a b";
  [%expect {| ["a "; "b"] |}];
  split ~rex:re_eow "a b";
  [%expect {| ["a"; " b"] |}];
  let rex = Re.Pcre.regexp "" in
  split ~rex "xx";
  [%expect {| ["x"; "x"] |}]
;;

let full_split ?max ~rex s =
  let res = Re.Pcre.full_split ?max ~rex s in
  Format.printf
    "[%a]@."
    Fmt.(
      list ~pp_sep:(Fmt.lit "; ") (fun fmt what ->
        match (what : Re.Pcre.split_result) with
        | Text s -> Format.fprintf fmt "Text %S" s
        | Delim s -> Format.fprintf fmt "Delim %S" s
        | NoGroup -> Format.fprintf fmt "NoGroup"
        | Group (x, s) -> Format.fprintf fmt "Group (%d, %S)" x s))
    res
;;

let%expect_test "full split" =
  (let full_split = full_split ~rex:(Re.Pcre.regexp "x(x)?") in
   full_split "testxxyyy";
   [%expect {| [Text "test"; Delim "xx"; Group (1, "x"); Text "yyy"] |}];
   full_split "testxyyy";
   [%expect {| [Text "test"; Delim "x"; NoGroup; Text "yyy"] |}]);
  let full_split = full_split ~rex:(Re.Pcre.regexp "[:_]") in
  full_split "";
  [%expect {| [] |}];
  full_split ~max:1 "xxx:yyy";
  [%expect {| [Text "xxx:yyy"] |}]
;;
