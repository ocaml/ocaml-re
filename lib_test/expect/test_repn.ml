open Import
open Re

let%expect_test "fixed repetition" =
  let re = Re.compile @@ Re.(repn (char 'a') 3 (Some 3)) in
  let test s = printf "%b\n" (Re.execp re s) in
  test "";
  [%expect {| false |}];
  test "aa";
  [%expect {| false |}];
  test "aaa";
  [%expect {| true |}];
  test "aaaa";
  [%expect {| true |}]
;;

let%expect_test "counted repetitions preserve overlapping search starts" =
  (* At the third 'a', the second copy and a new search attempt both await 'b',
     but their continuations differ. Deduplication must keep both paths. *)
  let repeated min = seq [ repn (str "ab") min (Some 2); eos ] in
  test_re (repeated 2) "ababab";
  test_re (repeated 0) "ababab";
  [%expect
    {|
    [| (2, 6) |]
    [| (2, 6) |]
    |}]
;;

let%expect_test "repn" =
  let a = char 'a' in
  test_re (repn a 0 None) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (repn a 2 None) "a";
  [%expect {| Not_found |}];
  test_re (repn a 2 None) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (repn a 0 (Some 0)) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (repn a 1 (Some 2)) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (repn a 1 (Some 2)) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (repn a 1 (Some 2)) "";
  [%expect {| Not_found |}];
  test_re (repn a 1 (Some 2)) "aaa";
  [%expect {| [| (0, 2) |] |}];
  invalid_argument (fun () -> repn empty (-1) None);
  [%expect {| Invalid_argument "Re.repn" |}];
  invalid_argument (fun () -> repn empty 1 (Some 0));
  [%expect {| Invalid_argument "Re.repn" |}];
  invalid_argument (fun () -> repn empty 4 (Some 3));
  [%expect {| Invalid_argument "Re.repn" |}]
;;

let%expect_test "rep1" =
  test_re (rep1 (char 'a')) "a";
  [%expect {| [| (0, 1) |] |}];
  test_re (rep1 (char 'a')) "aa";
  [%expect {| [| (0, 2) |] |}];
  test_re (rep1 (char 'a')) "";
  [%expect {| Not_found |}];
  test_re (rep1 (char 'a')) "b";
  [%expect {| Not_found |}]
;;

let%expect_test "opt" =
  test_re (opt (char 'a')) "";
  [%expect {| [| (0, 0) |] |}];
  test_re (opt (char 'a')) "a";
  [%expect {| [| (0, 1) |] |}]
;;

let copy s n =
  let len = String.length s in
  let b = Bytes.make (len * n) '\000' in
  for i = 0 to n - 1 do
    Bytes.blit_string s 0 b (i * len) len
  done;
  Bytes.to_string b
;;

let%expect_test "repeat sequence" =
  let s = "abcde" in
  let re = str s |> rep |> whole_string |> compile in
  for i = 0 to 3 do
    let r = copy s i in
    assert (Re.execp re r)
  done;
  [%expect {||}]
;;
