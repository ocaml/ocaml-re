open! Import
module Cset = Re_private.Cset
module Category = Re_private.Category
module Automata = Re_private.Automata

include struct
  open Automata
  module Ids = Ids
  module Working_area = Working_area
  module State = State

  let empty = empty
  let eps = eps
  let cst = cst
  let seq = seq
end

let pp_state state = print_dyn (State.to_dyn state)
let pp_expr fmt expr = Automata.pp fmt expr
let cat = Category.dummy

let str ids sem str =
  let rec loop (s : Char.t Seq.t) =
    match (s () : _ Seq.node) with
    | Nil -> eps ids
    | Cons (c, rest) ->
      let c = cst ids (Cset.csingle c) in
      seq ids sem c (loop rest)
  in
  loop (String.to_seq str)
;;

let rec loop wa d c =
  print_dyn (State.to_dyn d);
  match State.status d with
  | Failed -> Format.printf "> failed@."
  | Match _ -> Format.printf "> matched@."
  | Running ->
    let d = Automata.delta wa cat (Cset.of_char c) d in
    loop wa d c
;;

let%expect_test "string" =
  let re =
    let n = 4 in
    let s =
      let c = 'a' in
      String.make n c
    in
    let ids = Ids.create () in
    str ids `First s
  in
  let wa = Working_area.create () in
  loop wa (State.create cat re) 'a';
  [%expect
    {|
    ((TExp (first (Seq 97 97 97 97))))
    ((TExp (first (Seq 97 97 97))))
    ((TExp (first (Seq 97 97))))
    ((TExp 97))
    ((TExp Eps))
    ((TMarks ()))
    > matched
    |}];
  loop wa (State.create cat re) 'b';
  [%expect {|
    ((TExp (first (Seq 97 97 97 97))))
    ()
    > failed
    |}]
;;

let%expect_test "alternation" =
  let ids = Ids.create () in
  let re =
    let n = 4 in
    let s =
      let c = 'a' in
      String.make n c
    in
    List.init ~len:n ~f:(fun i ->
      let prefix = str ids `First s in
      let suffix =
        let c = Char.chr (Char.code 'b' + i) in
        cst ids (Cset.csingle c)
      in
      seq ids `First prefix suffix)
    |> Automata.alt ids
  in
  let wa = Working_area.create () in
  loop wa (State.create cat re) 'a';
  [%expect
    {|
    ((TExp
      (Alt (first (Seq (Seq 97 97 97 97) 98)) (first (Seq (Seq 97 97 97 97) 99))
       (first (Seq (Seq 97 97 97 97) 100)) (first (Seq (Seq 97 97 97 97) 101)))))
    ((first (TSeq ((TExp (Seq 97 97 97))) 98))
     (first (TSeq ((TExp (Seq 97 97 97))) 99))
     (first (TSeq ((TExp (Seq 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97))) 101)))
    ((first (TSeq ((TExp (Seq 97 97))) 98))
     (first (TSeq ((TExp (Seq 97 97))) 99))
     (first (TSeq ((TExp (Seq 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97))) 101)))
    ((first (TSeq ((TExp 97)) 98)) (first (TSeq ((TExp 97)) 99))
     (first (TSeq ((TExp 97)) 100)) (first (TSeq ((TExp 97)) 101)))
    ((TExp 98) (TExp 99) (TExp 100) (TExp 101))
    ()
    > failed
    |}]
;;

let%expect_test "alternation shared prefix" =
  let n = 4 in
  let ids = Ids.create () in
  let re =
    let prefix =
      let s =
        let c = 'a' in
        String.make n c
      in
      str ids `First s
    in
    let suffix =
      List.init ~len:n ~f:(fun i ->
        let c = Char.chr (Char.code 'b' + i) in
        cst ids (Cset.csingle c))
      |> Automata.alt ids
    in
    seq ids `First prefix suffix
  in
  let wa = Working_area.create () in
  loop wa (State.create cat re) 'a';
  [%expect
    {|
    ((TExp (first (Seq (Seq 97 97 97 97) (Alt 98 99 100 101)))))
    ((first (TSeq ((TExp (Seq 97 97 97))) (Alt 98 99 100 101))))
    ((first (TSeq ((TExp (Seq 97 97))) (Alt 98 99 100 101))))
    ((first (TSeq ((TExp 97)) (Alt 98 99 100 101))))
    ((TExp (Alt 98 99 100 101)))
    ()
    > failed
    |}]
;;
