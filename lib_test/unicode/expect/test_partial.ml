open Import

let t re s =
  let re = Re.compile re in
  let res = Re.exec_partial re s in
  Format.printf
    "`%s@."
    (match res with
     | `Partial -> "Partial"
     | `Full -> "Full"
     | `Mismatch -> "Mismatch")
;;

let%expect_test "partial matches" =
  let open Re in
  t (str "hello") "he";
  [%expect {| `Partial |}];
  t (str "hello") "goodbye";
  [%expect {| `Partial |}];
  (* exec_partial 3 should be `Full *)
  t (str "hello") "hello";
  [%expect {| `Partial |}];
  t (whole_string (str "hello")) "hello";
  [%expect {| `Partial |}];
  t (whole_string (str "hello")) "goodbye";
  [%expect {| `Mismatch |}];
  t (str "hello") "";
  [%expect {| `Partial |}];
  t (str "") "hello";
  [%expect {| `Full |}];
  t (whole_string (str "hello")) "";
  [%expect {| `Partial |}]
;;

let t = exec_partial_detailed

let%expect_test "partial detailed" =
  let open Re in
  t (str "hello") "he";
  [%expect {| `Partial 0 |}];
  (* Because of how the matching engine currently works, situations where
     the entirety of the input string cannot be a match like the test below
     actually return the last character as a potential start instead of just
     return `Partial (String.length input). This is still fine however as
     it still respects the mli contract, as no match could start before
     the given position, and is fine in practice as testing an extra
     character on extra input doesn't add much more in terms of workload.
  *)
  t (str "hello") "goodbye";
  [%expect {| `Partial 6 |}];
  t (str "hello") "hello";
  [%expect {| `Full [|0,5,"hello"|] |}];
  t (whole_string (str "hello")) "hello";
  [%expect {| `Full [|0,5,"hello"|] |}];
  t (whole_string (str "hello")) "goodbye";
  [%expect {| `Mismatch |}];
  t (str "hello") "";
  [%expect {| `Partial 0 |}];
  t (str "") "hello";
  [%expect {| `Full [|0,0,""|] |}];
  t (whole_string (str "hello")) "";
  [%expect {| `Partial 0 |}];
  t (str "abc") ".ab.ab";
  [%expect {| `Partial 4 |}];
  t ~pos:1 (seq [ not_boundary; str "b" ]) "ab";
  [%expect {| `Full [|1,2,"b"|] |}];
  t (seq [ group (str "a"); rep any; group (str "b") ]) ".acb.";
  [%expect {| `Full [|1,4,"acb";1,2,"a";3,4,"b"|] |}]
;;
