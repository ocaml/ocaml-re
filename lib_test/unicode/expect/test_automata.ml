open! Import

include struct
  module Automata = struct
    include Re_private_unicode.Automata
    include Re.Automata
  end

  let empty = Automata.empty
  let eps = Automata.eps
  let cst = Automata.cst
  let seq = Automata.seq
  let rep = Automata.rep
end

let pp_state state = print_dyn (Automata.State.to_dyn state)
let pp_expr fmt expr = Automata.pp fmt expr
let cat = Re.Category.dummy

let str ids sem str =
  let rec loop s =
    match (s () : _ Seq.node) with
    | Nil -> eps ids
    | Cons (c, rest) ->
      let c = cst ids (Cset.csingle c) in
      seq ids sem c (loop rest)
  in
  loop (Cset.Codec.to_seq str)

let loop ?(max = 100) wa d c =
  let cset = Cset.CodePage.(of_char c |> from_letter) in
  let rec loop d n =
    if n > 0 then (
      print_dyn (Automata.State.to_dyn d);
      match Automata.State.status_no_mutex d with
      | Failed -> Format.printf "> failed@."
      | Match _ -> Format.printf "> matched@."
      | Running ->
        let d = Automata.delta wa cat cset d in
        loop d (n - 1))
  in
  loop d max

let%expect_test "string" =
  let re =
    let n = 4 in
    let s =
      let c = 'a' in
      String.make n c
    in
    let ids = Automata.Ids.create () in
    str ids `First s
  in
  let wa = Automata.Working_area.create () in
  loop wa (Automata.State.create cat re) 'a';
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
  loop wa (Automata.State.create cat re) 'b';
  [%expect {|
    ((TExp (first (Seq 97 97 97 97))))
    ()
    > failed
    |}]

let%expect_test "alternation" =
  let re =
    let ids = Automata.Ids.create () in
    let n = 4 in
    let s =
      let c = 'a' in
      String.make n c
    in
    List.init ~len:n ~f:(fun i ->
      let prefix = str ids `First s in
      let suffix =
        let c = Char.chr (Char.code 'b' + i) in
        cst ids (Cset.CodePage.of_char c |> Cset.csingle)
      in
      seq ids `First prefix suffix)
    |> Automata.alt ids
  in
  let wa = Automata.Working_area.create () in
  loop wa (Automata.State.create cat re) 'a';
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

let%expect_test "alternation shared prefix" =
  let n = 4 in
  let re =
    let ids = Automata.Ids.create () in
    let prefix =
      let s =
        let c = 'a' in
        String.make n c
      in
      str ids `First s
    in
    let suffix =
      List.init ~len:n ~f:(fun i ->
        let c =
          Cset.CodePage.of_int
            (Cset.CodePage.(of_char 'b' |> from_letter |> to_int) + i)
        in
        cst ids (Cset.CodePage.to_letter c |> Cset.csingle))
      |> Automata.alt ids
    in
    seq ids `First prefix suffix
  in
  let wa = Automata.Working_area.create () in
  loop wa (Automata.State.create cat re) 'a';
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

let%expect_test "kleene star" =
  let re =
    let ids = Automata.Ids.create () in
    rep ids `Greedy `First (cst ids (Cset.CodePage.of_char 'z' |> Cset.csingle))
  in
  let wa = Automata.Working_area.create () in
  loop ~max:4 wa (Automata.State.create cat re) 'z';
  [%expect
    {|
    ((TExp (first (Rep 122))))
    ((TExp (first (Rep 122))) (TMarks ()))
    ((TExp (first (Rep 122))) (TMarks ()))
    ((TExp (first (Rep 122))) (TMarks ()))
    |}];
  loop ~max:3 wa (Automata.State.create cat re) 'a';
  [%expect
    {|
    ((TExp (first (Rep 122))))
    ((TMarks ()))
    > matched
    |}]

let%expect_test "derivative recomputation" =
  let sem = `Longest in
  let re =
    let ids = Automata.Ids.create () in
    (* let lhs = rep ids `Non_greedy sem (cst ids Cset.cany) in *)
    let lhs =
      rep ids `Non_greedy sem
        (cst ids
           (Cset.cseq
              Cset.CodePage.(of_int 0 |> to_letter)
              Cset.CodePage.(of_int 255 |> to_letter)))
    in
    let rhs =
      seq ids sem
        (Automata.mark ids Automata.Mark.start)
        (Automata.alt ids
           [
             cst ids (Cset.CodePage.of_char 'z' |> Cset.csingle);
             cst ids (Cset.CodePage.of_char 'b' |> Cset.csingle);
           ])
    in
    seq ids sem lhs rhs
  in
  let wa = Automata.Working_area.create () in
  loop ~max:7 wa (Automata.State.create cat re) 'z';
  [%expect
    {|
    ((TExp (long (Seq (Rep ((0 255))) (Seq (Mark 0) (Alt 122 98))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 0)))) Eps))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 1)))) Eps) (TMarks ((marks ((0 0))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 0)))) Eps) (TMarks ((marks ((0 1))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 1)))) Eps) (TMarks ((marks ((0 0))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 0)))) Eps) (TMarks ((marks ((0 1))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98))))
     (TExp ((marks ((0 1)))) Eps) (TMarks ((marks ((0 0))))))
    |}];
  loop ~max:7 wa (Automata.State.create cat re) 'a';
  [%expect
    {|
    ((TExp (long (Seq (Rep ((0 255))) (Seq (Mark 0) (Alt 122 98))))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    ((long (TSeq ((TExp (Rep ((0 255))))) (Seq (Mark 0) (Alt 122 98)))))
    |}]
