module Cset = Re_private.Cset
module Bit_vector = Re_private.Bit_vector
module Hash_set = Re_private.Hash_set

let invalid f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "expected Invalid_argument"
;;

let%expect_test "prepend partitions preserve membership and payload order" =
  let original = [ Cset.cseq '\000' '\002', [ 1 ]; Cset.cseq '\004' '\006', [ 2 ] ] in
  for mask = 0 to 255 do
    let source = ref Cset.empty in
    for i = 0 to 7 do
      if mask land (1 lsl i) <> 0 then source := Cset.add (Cset.of_int i) !source
    done;
    let result = Cset.prepend !source [ 0 ] original in
    let previous = ref (-1) in
    List.iter
      (fun (set, _) ->
         match Cset.fold_right set ~init:[] ~f:(fun a b acc -> (a, b) :: acc) with
         | [ (a, b) ] ->
           assert (Cset.to_int a > !previous);
           previous := Cset.to_int b
         | _ -> assert false)
      result;
    for i = 0 to 7 do
      let payloads parts =
        List.filter_map
          (fun (set, payload) ->
             if Cset.mem (Cset.of_int i) set then Some payload else None)
          parts
      in
      let expected =
        List.map
          (fun payload -> if mask land (1 lsl i) <> 0 then 0 :: payload else payload)
          (payloads original)
      in
      assert (payloads result = expected)
    done
  done;
  assert (Cset.prepend Cset.cany [ 0 ] [] = []);
  [%expect {| |}]
;;

let%expect_test "bit vectors at every byte boundary" =
  for length = 0 to 33 do
    let vector = Bit_vector.create_zero length in
    let model = Array.make length false in
    let check () =
      assert (Bit_vector.length vector = length);
      Array.iteri (fun i expected -> assert (Bit_vector.get vector i = expected)) model
    in
    check ();
    for i = 0 to length - 1 do
      Bit_vector.set vector i true;
      model.(i) <- true;
      check ()
    done;
    for i = length - 1 downto 0 do
      Bit_vector.set vector i false;
      model.(i) <- false;
      check ()
    done;
    for i = 0 to length - 1 do
      Bit_vector.set vector i true
    done;
    Bit_vector.reset_zero vector;
    check ();
    List.iter
      (fun i ->
         invalid (fun () -> Bit_vector.get vector i);
         invalid (fun () -> Bit_vector.set vector i true))
      [ -1; length; length + 1 ]
  done;
  [%expect {| |}]
;;

let%expect_test "hash set operation sequences, collisions, growth and reuse" =
  let run operations =
    let set = Hash_set.create () in
    let model = ref [] in
    List.iter
      (fun op ->
         (match op with
          | None ->
            Hash_set.clear set;
            model := []
          | Some x ->
            Hash_set.add set x;
            model := x :: !model);
         assert (Hash_set.is_empty set = (!model = []));
         for x = 0 to 64 do
           assert (Hash_set.mem set x = List.mem x !model)
         done;
         List.iter (fun x -> assert (Hash_set.mem set x)) !model)
      operations
  in
  let rec sequences n prefix =
    if n = 0
    then run (List.rev prefix)
    else List.iter (fun op -> sequences (n - 1) (op :: prefix)) [ None; Some 0; Some 1 ]
  in
  sequences 5 [];
  let collisions =
    List.init 4096 Fun.id |> List.filter (fun i -> Hashtbl.hash i land 63 = 0)
  in
  let additions = List.map (fun i -> Some i) collisions in
  run (additions @ [ None; None ] @ additions);
  run (List.init 256 (fun i -> if i mod 31 = 0 then None else Some (i mod 64)));
  (* Existing tests only print on failure. Exercise the successful diagnostic too. *)
  let set = Hash_set.create () in
  assert (Format.asprintf "%a" Hash_set.pp set = "(table )\n(size 0)\n");
  [%expect {| |}]
;;
