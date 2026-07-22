open Import
open Re

(* Capturing subtle tests that were found by fuzzing. *)

let%expect_test "predecence of seq (rep .., rep ..)" =
  (* was: [| (0, 1); (0, 1) |]
     ref: [| (0, 1); (0, 0) |] *)
  let re = longest (seq [ group (non_greedy (rep any)); rep any ]) in
  Format.printf "%a@." pp_re (Re.compile re);
  test_re re "a";
  [%expect
    {|
    (seq short (rep Greedy first (cst 0))
       (seq long
          (seq first (mark 0)
             (seq first
                (seq first
                   (seq first (mark 2)
                      (seq first (rep Non_greedy first (cst 0)) (mark 3)))
                   (rep Greedy first (cst 0)))
                (mark 1)))
          eps))
    [| (0, 1); (0, 0) |]
    |}]
;;

let%expect_test "precedence of rep (rep ..)" =
  let re = longest (non_greedy (rep (group (rep any)))) in
  Format.printf "%a@." pp_re (Re.compile re);
  test_re re "aa";
  (* was: [[0; 2]; [0; 2]]
     ref: [[0; 2]; [1; 2]] *)
  [%expect
    {|
    (seq short (rep Greedy first (cst 0))
       (seq long
          (seq first (mark 0)
             (seq first
                (rep Non_greedy first
                   (seq first (mark 2)
                      (seq first (rep Non_greedy first (cst 0)) (mark 3))))
                (mark 1)))
          eps))
    [| (0, 2); (1, 2) |]
    |}]
;;
