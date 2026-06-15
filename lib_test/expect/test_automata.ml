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
  let rep = rep
end

let pp_state state = print_dyn (State.to_dyn state)
let pp_expr fmt expr = Automata.pp fmt expr
let cat = Category.from_char '\000'

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

let loop ?(max = 100) wa d c =
  let cset = Cset.of_char c in
  let rec loop d n =
    if n > 0
    then (
      print_dyn (State.to_dyn d);
      match State.status_no_mutex d with
      | Failed -> Format.printf "> failed@."
      | Match _ -> Format.printf "> matched@."
      | Running ->
        let d = Automata.delta wa cat cset d in
        loop d (n - 1))
  in
  loop d max
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
    ((TExp (Seq:F 97 97 97 97)))
    ((TExp (Seq:F 97 97 97)))
    ((TExp (Seq:F 97 97)))
    ((TExp 97))
    ((TExp Eps))
    ((TMatch ()))
    > matched
    |}];
  loop wa (State.create cat re) 'b';
  [%expect {|
    ((TExp (Seq:F 97 97 97 97)))
    ()
    > failed
    |}]
;;

let%expect_test "alternation" =
  let re =
    let ids = Ids.create () in
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
      (Alt (Seq:F (Seq:F 97 97 97 97) 98) (Seq:F (Seq:F 97 97 97 97) 99)
       (Seq:F (Seq:F 97 97 97 97) 100) (Seq:F (Seq:F 97 97 97 97) 101))))
    ((TSeq:F ((TExp (Seq:F 97 97 97))) 98) (TSeq:F ((TExp (Seq:F 97 97 97))) 99)
     (TSeq:F ((TExp (Seq:F 97 97 97))) 100)
     (TSeq:F ((TExp (Seq:F 97 97 97))) 101))
    ((TSeq:F ((TExp (Seq:F 97 97))) 98) (TSeq:F ((TExp (Seq:F 97 97))) 99)
     (TSeq:F ((TExp (Seq:F 97 97))) 100) (TSeq:F ((TExp (Seq:F 97 97))) 101))
    ((TSeq:F ((TExp 97)) 98) (TSeq:F ((TExp 97)) 99) (TSeq:F ((TExp 97)) 100)
     (TSeq:F ((TExp 97)) 101))
    ((TExp 98) (TExp 99) (TExp 100) (TExp 101))
    ()
    > failed
    |}]
;;

let%expect_test "alternation shared prefix" =
  let n = 4 in
  let re =
    let ids = Ids.create () in
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
    ((TExp (Seq:F (Seq:F 97 97 97 97) (Alt 98 99 100 101))))
    ((TSeq:F ((TExp (Seq:F 97 97 97))) (Alt 98 99 100 101)))
    ((TSeq:F ((TExp (Seq:F 97 97))) (Alt 98 99 100 101)))
    ((TSeq:F ((TExp 97)) (Alt 98 99 100 101)))
    ((TExp (Alt 98 99 100 101)))
    ()
    > failed
    |}]
;;

let%expect_test "kleene star" =
  let re =
    let ids = Ids.create () in
    rep ids `Greedy `First (cst ids (Cset.csingle 'z'))
  in
  let wa = Working_area.create () in
  loop ~max:4 wa (State.create cat re) 'z';
  [%expect
    {|
    ((TExp (Rep:GF 122)))
    ((TExp (Rep:GF 122)) (TMatch ()))
    ((TExp (Rep:GF 122)) (TMatch ()))
    ((TExp (Rep:GF 122)) (TMatch ()))
    |}];
  loop ~max:3 wa (State.create cat re) 'a';
  [%expect {|
    ((TExp (Rep:GF 122)))
    ((TMatch ()))
    > matched
    |}]
;;

let%expect_test "derivative recomputation" =
  let sem = `Longest in
  let re =
    let ids = Ids.create () in
    let lhs = rep ids `Non_greedy sem (cst ids Cset.cany) in
    let rhs =
      seq
        ids
        sem
        (Automata.mark ids Automata.Mark.start)
        (Automata.alt ids [ cst ids (Cset.csingle 'z'); cst ids (Cset.csingle 'b') ])
    in
    seq ids sem lhs rhs
  in
  let wa = Working_area.create () in
  loop ~max:7 wa (State.create cat re) 'z';
  [%expect
    {|
    ((TExp (Seq:L (Rep:NL ((0 255))) (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 0)))) Eps))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 1)))) Eps) (TMatch ((marks ((0 0))))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 0)))) Eps) (TMatch ((marks ((0 1))))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 1)))) Eps) (TMatch ((marks ((0 0))))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 0)))) Eps) (TMatch ((marks ((0 1))))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98)))
     (TExp ((marks ((0 1)))) Eps) (TMatch ((marks ((0 0))))))
    |}];
  loop ~max:7 wa (State.create cat re) 'a';
  [%expect
    {|
    ((TExp (Seq:L (Rep:NL ((0 255))) (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    ((TSeq:L ((TExp (Rep:NL ((0 255))))) (Seq:L (Mark 0) (Alt 122 98))))
    |}]
;;
