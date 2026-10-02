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
  t (str "hello") "hello";
  [%expect {| `Full |}];
  t (whole_string (str "hello")) "hello";
  [%expect {| `Partial |}];
  t (whole_string (str "hello")) "goodbye";
  [%expect {| `Mismatch |}];
  t (str "hello") "";
  [%expect {| `Partial |}];
  t (str "") "hello";
  [%expect {| `Full |}];
  t (whole_string (str "hello")) "";
  [%expect {| `Partial |}];
  t (alt [ str "ab"; str "a" ]) "a";
  [%expect {| `Partial |}];
  t (seq [ str "ab"; bos ]) "ab";
  [%expect {| `Mismatch |}];
  t (seq [ str "ab"; eos ]) "ab";
  [%expect {| `Partial |}];
  ()
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
  [%expect {| `Partial 0 |}];
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
  t (seq [ group (str "a"); shortest (rep any); group (str "b") ]) ".acb.";
  [%expect {| `Full [|1,4,"acb";1,2,"a";3,4,"b"|] |}];
  t (alt [ str "ab"; str "a" ]) "a";
  [%expect {| `Partial 0 |}];
  t (seq [ str "ab"; bos ]) "ab";
  [%expect {| `Mismatch |}];
  t (seq [ str "ab"; eos ]) "ab";
  [%expect {| `Partial 0 |}];
  ()
;;

let%expect_test "partial positions are not conservative with bounded repetition" =
  let open Re in
  (* [`Partial n] promises that no match could start before [n]. Extending the
     input below makes a match start at 2, but the reported position is 3. *)
  let pat = longest (seq [ greedy (repn any 0 (Some 3)); stop ]) in
  t pat "\224.\192\192";
  [%expect {| `Partial 3 |}];
  (match exec_opt (compile pat) "\224.\192\1920" with
   | None -> print_endline "None"
   | Some g -> Printf.printf "match starts at %d\n" (Group.start g 0));
  [%expect {| match starts at 2 |}];
  ()
;;

let%expect_test "leol matches before a final newline are partial" =
  let open Re in
  (* [Full] promises that the match survives every extension. A match that relied
     on the final newline does not: appending another newline moves it. *)
  t (group leol) "a\n";
  [%expect {| `Partial 0 |}];
  t (seq [ str "a"; leol ]) "a\n";
  [%expect {| `Partial 0 |}];
  t (seq [ str "a"; leol ]) "a\nb";
  [%expect {| `Partial 2 |}];
  (* [eol] does not depend on the end of input and stays definite. *)
  t (group eol) "a\n";
  [%expect {| `Full [|1,1,"";1,1,""|] |}];
  ()
;;
