open Import

(* Record current Re.Pcre behavior, including compatibility bugs, without
   requiring a native PCRE dependency. Update the snapshots with the fixes. *)
let split pattern input =
  Format.printf "split %S %S: " pattern input;
  Re.Pcre.split ~rex:(Re.Pcre.regexp pattern) input |> strings
;;

let full_split ?(max = 0) pattern input =
  let result = Re.Pcre.full_split ~max ~rex:(Re.Pcre.regexp pattern) input in
  Format.printf
    "full_split ~max:%d %S %S: [%a]@."
    max
    pattern
    input
    Fmt.(
      list ~pp_sep:(lit "; ") (fun fmt (token : Re.Pcre.split_result) ->
        match token with
        | Text s -> Format.fprintf fmt "Text %S" s
        | Delim s -> Format.fprintf fmt "Delim %S" s
        | Group (i, s) -> Format.fprintf fmt "Group (%d, %S)" i s
        | NoGroup -> Format.fprintf fmt "NoGroup"))
    result
;;

let%expect_test "full_split limits" =
  (* max=2 should leave "b,c" as the last text in the first example. *)
  full_split ~max:2 "," "a,b,c";
  full_split ~max:2 "," ",a,,b";
  full_split ~max:2 "(,)" "a,b,c";
  full_split ~max:2 "," "a,";
  full_split ~max:1 "," "a,b";
  full_split ~max:3 "," "a,b,c,d";
  full_split ~max:10 "," "a,b";
  full_split ~max:2 "," "abc";
  full_split ~max:2 "," ",";
  full_split ~max:2 "xx" "axxbxxc";
  full_split ~max:2 "," "a,b,";
  [%expect
    {|
    full_split ~max:2 "," "a,b,c": [Text "a"; Delim ","; Text "b,c"]
    full_split ~max:2 "," ",a,,b": [Delim ","; Text "a"; Delim ","; Text ",b"]
    full_split ~max:2 "(,)" "a,b,c": [Text "a"; Delim ","; Group (1, ","); Text "b,c"]
    full_split ~max:2 "," "a,": [Text "a"; Delim ","]
    full_split ~max:1 "," "a,b": [Text "a,b"]
    full_split ~max:3 "," "a,b,c,d": [Text "a"; Delim ","; Text "b"; Delim ","; Text "c,d"]
    full_split ~max:10 "," "a,b": [Text "a"; Delim ","; Text "b"]
    full_split ~max:2 "," "abc": [Text "abc"]
    full_split ~max:2 "," ",": [Delim ","]
    full_split ~max:2 "xx" "axxbxxc": [Text "a"; Delim "xx"; Text "bxxc"]
    full_split ~max:2 "," "a,b,": [Text "a"; Delim ","; Text "b,"]
    |}]
;;

let%expect_test "split delimiter captures" =
  (* Captures should be included, including an empty field for an absent group. *)
  split "(,)" "a,b";
  split "(,)|(x)" "a,b";
  split "(,)" "a,";
  split "()" "ab";
  [%expect
    {|
    split "(,)" "a,b": ["a"; "b"]
    split "(,)|(x)" "a,b": ["a"; "b"]
    split "(,)" "a,": ["a"]
    split "()" "ab": ["a"; "b"]
    |}]
;;

let%expect_test "full_split trailing delimiters" =
  (* The default max=0 should strip trailing delimiters without captures. *)
  full_split "," "a,";
  full_split "," ",,";
  full_split ~max:(-1) "," "a,";
  full_split "(,)" "a,";
  [%expect
    {|
    full_split ~max:0 "," "a,": [Text "a"; Delim ","]
    full_split ~max:0 "," ",,": [Delim ","; Text ""; Delim ","]
    full_split ~max:-1 "," "a,": [Text "a"; Delim ","]
    full_split ~max:0 "(,)" "a,": [Text "a"; Delim ","; Group (1, ",")]
    |}]
;;

let%expect_test "full_split adjacent delimiters" =
  (* Adjacent delimiters should not introduce an empty Text token. *)
  full_split "," "a,,b";
  [%expect
    {| full_split ~max:0 "," "a,,b": [Text "a"; Delim ","; Text ""; Delim ","; Text "b"] |}]
;;

let%expect_test "split empty alternatives" =
  (* Retry a nonempty match at the same position before advancing past an empty
     delimiter: the first example should produce [""; "b"]. *)
  List.iter [ "(?:|a)"; "^|a"; "a*?" ] ~f:(fun pattern -> split pattern "ab");
  split "(?:|ab)" "abc";
  split "(|a)" "ab";
  split "(?:|a)" "ba";
  let fields = Re.Pcre.split ~rex:(Re.Pcre.regexp "") (String.make 10000 'x') in
  Format.printf "empty pattern, 10000 bytes: %d fields@." (List.length fields);
  [%expect
    {|
    split "(?:|a)" "ab": ["a"; "b"]
    split "^|a" "ab": ["ab"]
    split "a*?" "ab": ["a"; "b"]
    split "(?:|ab)" "abc": ["a"; "b"; "c"]
    split "(|a)" "ab": ["a"; "b"]
    split "(?:|a)" "ba": ["b"; "a"]
    empty pattern, 10000 bytes: 10000 fields
    |}]
;;
