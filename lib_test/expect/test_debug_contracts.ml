open Import
module Ast = Re_private.Ast
module A = Re_private.Automata
module Cset = Re_private.Cset
module Category = Re_private.Category

let%expect_test "AST diagnostics distinguish every constructor" =
  let open Re in
  let cases =
    [ empty
    ; epsilon
    ; seq [ bos; eos ]
    ; repn (str "ab") 2 (Some 4)
    ; rep bos
    ; bol
    ; eol
    ; bow
    ; eow
    ; not_boundary
    ; bos
    ; eos
    ; leol
    ; start
    ; stop
    ; alt [ bos; eos ]
    ; group bos
    ; group ~name:"named" eos
    ; no_group (group bos)
    ; nest bos
    ; first bos
    ; shortest bos
    ; longest bos
    ; greedy bos
    ; non_greedy bos
    ; case bos
    ; no_case bos
    ; case (char 'a')
    ; no_case (char 'a')
    ; inter [ char 'a'; char 'b' ]
    ; compl [ char 'a' ]
    ; diff (char 'a') (char 'b')
    ]
  in
  List.iter cases ~f:(fun r ->
    Format.printf "%a@." Re.pp r;
    print_dyn (Ast.to_dyn r));
  [%expect
    {|
    (Alternative )
    (Ast Alternative)
    (Sequence )
    Sequence
    (Sequence Beg_of_strEnd_of_str)
    (Sequence Beg_of_str End_of_str)
    (Repeat (Sequence (Set 97)(Set 98)) 2 4)
    (Repeat (Sequence (Set (Cset 97)) (Set (Cset 98))) 2 4)
    (Repeat Beg_of_str 0)
    (Repeat Beg_of_str 0)
    Beg_of_line
    Beg_of_line
    End_of_line
    End_of_line
    Beg_of_word
    Beg_of_word
    End_of_word
    End_of_word
    Not_bound
    Not_bound
    Beg_of_str
    Beg_of_str
    End_of_str
    End_of_str
    Last_end_of_line
    Last_end_of_line
    Start
    Start
    Stop
    Stop
    (Alternative Beg_of_strEnd_of_str)
    (Ast (Alternative Beg_of_str End_of_str))
    (Group Beg_of_str)
    (Group Beg_of_str)
    (Named_group named End_of_str)
    (Group named End_of_str)
    (No_group (Group Beg_of_str))
    (No_group (Group Beg_of_str))
    (Nest Beg_of_str)
    (Nest Beg_of_str)
    (Sem first Beg_of_str)
    (Sem first Beg_of_str)
    (Sem short Beg_of_str)
    (Sem short Beg_of_str)
    (Sem long Beg_of_str)
    (Sem long Beg_of_str)
    (Sem_greedy Greedy Beg_of_str)
    (Sem_greedy Greedy Beg_of_str)
    (Sem_greedy Non_greedy Beg_of_str)
    (Sem_greedy Non_greedy Beg_of_str)
    (Case Beg_of_str)
    (Ast (Case Beg_of_str))
    (No_case Beg_of_str)
    (Ast (No_case Beg_of_str))
    (Case (Set 97))
    (Set (Cast (Case (Cset 97))))
    (No_case (Set 97))
    (Set (Cast (No_case (Cset 97))))
    (Intersection (Set 97)(Set 98))
    (Set (Intersection (Cset 97) (Cset 98)))
    (Complement (Set 97))
    (Set (Complement (Cset 97)))
    (Difference (Set 97) (Set 98))
    (Set (Difference (Cset 97) (Cset 98)))
    |}]
;;

let%expect_test "mark diagnostics preserve the generated identifier" =
  let m, r = Re.mark Re.bos in
  let expected =
    Dyn.variant "Pmark" [ Re_private.Pmark.to_dyn m; Dyn.enum "Beg_of_str" ]
  in
  assert (Poly.equal (Ast.to_dyn r) expected);
  let expected = Printf.sprintf "(Pmark %d Beg_of_str)" (m :> int) in
  assert (String.equal (Format.asprintf "%a" Re.pp r) expected);
  [%expect {| |}]
;;

let%expect_test "automata expression, state and cached status diagnostics" =
  let ids = A.Ids.create () in
  let a = A.cst ids (Cset.csingle 'a') in
  let mark = A.Mark.start in
  let pmark = Re_private.Pmark.gen () in
  let exprs =
    [ A.empty ids
    ; A.eps ids
    ; a
    ; A.alt ids [ a; A.eps ids ]
    ; A.seq ids `Longest a (A.rep ids `Non_greedy `Shortest a)
    ; A.mark ids mark
    ; A.erase ids mark (A.Mark.next mark)
    ; A.before ids Category.inexistant
    ; A.after ids Category.inexistant
    ]
  in
  List.iter exprs ~f:(fun expr ->
    Format.printf "%a@." A.pp expr;
    print_dyn (A.to_dyn expr));
  let marked = A.pmark ids pmark in
  assert (
    Poly.equal (A.to_dyn marked) (Dyn.variant "Pmark" [ Re_private.Pmark.to_dyn pmark ]));
  assert (
    String.equal
      (Format.asprintf "%a" A.pp marked)
      (Printf.sprintf "(pmark %d)" (pmark :> int)));
  let wa = A.Working_area.create () in
  let state = A.State.create Category.inexistant (A.seq ids `First (A.mark ids mark) a) in
  let check_cached state =
    let first = A.State.status_no_mutex state in
    assert (Phys_equal.equal first (A.State.status_no_mutex state));
    first
  in
  assert (Poly.equal (check_cached A.State.dummy) A.Status.Failed);
  let failed =
    A.delta
      wa
      (Category.from_char 'b')
      (Cset.of_char 'b')
      (A.State.create Category.inexistant a)
  in
  assert (Poly.equal (check_cached failed) A.Status.Failed);
  assert (Poly.equal (check_cached state) A.Status.Running);
  Format.printf "%a@." A.State.pp state;
  let state = A.delta wa (Category.from_char 'a') (Cset.of_char 'a') state in
  Format.printf "%a@." A.State.pp state;
  let state = A.delta wa Category.inexistant Cset.null_char state in
  assert (
    match check_cached state with
    | Match _ -> true
    | Running | Failed -> false);
  Format.printf "%a@." A.State.pp state;
  let expr = A.seq ids `Longest (A.rep ids `Greedy `First a) a in
  let state = A.State.create Category.inexistant expr in
  let state = A.delta wa (Category.from_char 'a') (Cset.of_char 'a') state in
  Format.printf "%a@." A.State.pp state;
  let state = A.State.create Category.inexistant marked in
  let state = A.delta wa Category.inexistant Cset.null_char state in
  assert (
    match check_cached state with
    | Match (_, pmarks) -> Re_private.Pmark.Set.mem pmark pmarks
    | Running | Failed -> false);
  assert (
    String.equal
      (Format.asprintf "%a" A.State.pp state)
      (Printf.sprintf "[(TMatch pmarks %d)]" (pmark :> int)));
  [%expect
    {|
    (alt )
    Alt
    eps
    Eps
    (cst 97)
    97
    (alt (cst 97)eps)
    (Alt 97 Eps)
    (seq long (cst 97) (rep Non_greedy short (cst 97)))
    (Seq:L 97 (Rep:NS 97))
    (mark 0)
    (Mark 0)
    (erase 0 1)
    (Erase 0 1)
    (before 1)
    (Before 1)
    (after 1)
    (After 1)
    [(TExp 14 () (seq first (mark 0) (cst 97)))]
    [(TExp 0 (marks 0-0) (eps))]
    [(TMatch marks 0-0)]
    [(TSeq long (TExp 15 () (rep Greedy first (cst 97))) (cst 97));
    (TExp 0 () (eps))]
    |}]
;;

let%expect_test "dynamic values preserve payloads" =
  assert (
    Poly.equal (Dyn.result Dyn.int Dyn.string (Ok 1)) (Dyn.variant "Ok" [ Dyn.int 1 ]));
  assert (
    Poly.equal
      (Dyn.result Dyn.int Dyn.string (Error "x"))
      (Dyn.variant "Error" [ Dyn.string "x" ]));
  assert (Poly.equal (Dyn.option Dyn.int None) (Dyn.enum "None"));
  assert (Poly.equal (Dyn.option Dyn.int (Some 2)) (Dyn.variant "Some" [ Dyn.int 2 ]));
  List.iter
    [ `First, "first"; `Shortest, "short"; `Longest, "long" ]
    ~f:(fun (sem, expected) -> assert (Poly.equal (A.Sem.to_dyn sem) (Dyn.enum expected)));
  List.iter
    [ `Greedy, "Greedy"; `Non_greedy, "Non_greedy" ]
    ~f:(fun (kind, expected) ->
      assert (Poly.equal (A.Rep_kind.to_dyn kind) (Dyn.enum expected)));
  let re = Re.compile Re.(seq [ bos; group (char 'a'); eos ]) in
  let initial = Re_private.Compile.to_dyn re in
  (match Re_private.Compile.to_dyn ~color_map:true re with
   | Dyn.Record fields -> assert (Poly.equal (List.assoc "initial" fields) initial)
   | _ -> assert false);
  [%expect {| |}]
;;
