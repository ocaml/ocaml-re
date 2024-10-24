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

let pp_state fmt state = Base.Sexp.pp_hum fmt (sexp_of_dyn (State.to_dyn state))
let pp_expr fmt expr = Automata.pp fmt expr
let cat = Category.dummy

let str ids sem str =
  let rec loop (s : Stdlib.Char.t Seq.t) =
    match s () with
    | Seq.Nil -> eps ids
    | Seq.Cons (c, rest) ->
      let c = cst ids (Cset.csingle c) in
      seq ids sem c (loop rest)
  in
  loop (String.to_seq str)
;;

let%expect_test "string" =
  let c = 'a' in
  let n = 4 in
  let s = String.make n c in
  let ids = Ids.create () in
  let re = str ids `First s in
  let wa = Working_area.create () in
  let rec loop d c =
    Format.printf "%a@." pp_state d;
    match State.status d with
    | Failed -> Format.printf "> failed@."
    | Match _ -> Format.printf "> matched@."
    | Running ->
      let d = Automata.delta wa cat (Cset.of_char c) d in
      loop d c
  in
  loop (State.create cat re) 'a';
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
  loop (State.create cat re) 'b';
  [%expect {|
    ((TExp (first (Seq 97 97 97 97))))
    ()
    > failed
    |}]
;;

let%expect_test "alternation" =
  let c = 'a' in
  let n = 5 in
  let s = String.make n c in
  let ids = Ids.create () in
  let re =
    Automata.alt
      ids
      (List.init ~len:n ~f:(fun _ ->
         let prefix = str ids `First s in
         let suffix = cst ids (Cset.csingle 'd') in
         seq ids `First prefix suffix))
  in
  let wa = Working_area.create () in
  let rec loop d c =
    Format.printf "%a@." pp_state d;
    match State.status d with
    | Failed -> Format.printf "> failed@."
    | Match _ -> Format.printf "> matched@."
    | Running ->
      let d = Automata.delta wa cat (Cset.of_char c) d in
      loop d c
  in
  loop (State.create cat re) 'a';
  [%expect
    {|
    ((TExp
      (Alt (first (Seq (Seq 97 97 97 97 97) 100))
       (first (Seq (Seq 97 97 97 97 97) 100))
       (first (Seq (Seq 97 97 97 97 97) 100))
       (first (Seq (Seq 97 97 97 97 97) 100))
       (first (Seq (Seq 97 97 97 97 97) 100)))))
    ((first (TSeq ((TExp (Seq 97 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97 97))) 100)))
    ((first (TSeq ((TExp (Seq 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97 97))) 100)))
    ((first (TSeq ((TExp (Seq 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97))) 100))
     (first (TSeq ((TExp (Seq 97 97))) 100)))
    ((first (TSeq ((TExp 97)) 100)) (first (TSeq ((TExp 97)) 100))
     (first (TSeq ((TExp 97)) 100)) (first (TSeq ((TExp 97)) 100))
     (first (TSeq ((TExp 97)) 100)))
    ((TExp 100) (TExp 100) (TExp 100) (TExp 100) (TExp 100))
    ()
    > failed
    |}]
;;
