module Cset = Re_private.Cset
module Re = Re_private.Re

let%expect_test "a subtracted interval can cover several disjoint source intervals" =
  assert (Cset.is_empty (Cset.diff (Cset.set "ac") (Cset.cseq 'a' 'c')));
  let re = Re.(compile (diff (set "ac") (rg 'a' 'c'))) in
  assert (not (Re.execp re "a"));
  assert (not (Re.execp re "c"));
  [%expect {| |}]
;;

let%expect_test "set algebra agrees with every pair of subsets of eight bytes" =
  (* Shift the same exhaustive model to both edges of the byte alphabet. *)
  List.iter
    (fun base ->
       let byte i = Cset.of_int (base + i) in
       let sets =
         Array.init 256 (fun mask ->
           let set = ref Cset.empty in
           for i = 0 to 7 do
             if mask land (1 lsl i) <> 0 then set := Cset.add (byte i) !set
           done;
           !set)
       in
       Array.iteri
         (fun a x ->
            Array.iteri
              (fun b y ->
                 let union = Cset.union x y in
                 let inter = Cset.inter x y in
                 let diff = Cset.diff x y in
                 for i = 0 to 7 do
                   let ax = a land (1 lsl i) <> 0 in
                   let by = b land (1 lsl i) <> 0 in
                   List.iter
                     (fun (name, set, expected) ->
                        if Cset.mem (byte i) set <> expected
                        then
                          failwith
                            (Printf.sprintf
                               "%s: base=%d a=%d b=%d bit=%d"
                               name
                               base
                               a
                               b
                               i))
                     [ "union", union, ax || by
                     ; "inter", inter, ax && by
                     ; "diff", diff, ax && not by
                     ]
                 done;
                 assert (Cset.equal x y = (a = b));
                 assert (Cset.compare x y = 0 = (a = b)))
              sets;
            let previous = ref (base - 2) in
            Cset.fold_right x ~init:[] ~f:(fun a b acc -> (a, b) :: acc)
            |> List.iter (fun (a, b) ->
              assert (Cset.to_int a > !previous + 1);
              assert (Cset.to_int a <= Cset.to_int b);
              previous := Cset.to_int b);
            if a <> 0 then assert (Cset.mem (Cset.pick x) x))
         sets)
    [ 0; 248 ];
  (match Cset.pick Cset.empty with
   | exception Invalid_argument _ -> ()
   | _ -> assert false);
  [%expect {| |}]
;;
