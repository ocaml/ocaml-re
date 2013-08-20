(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

type sem = [ `Longest | `Shortest | `First ]

type rep_kind = [ `Greedy | `Non_greedy ]

type category = int
type mark = int
type idx = int

type expr = { id : int; def : def }

and def =
    Cst of Cset.t
  | Alt of expr list
  | Seq of sem * expr * expr
  | Eps
  | Rep of rep_kind * sem * expr
  | Mark of int
  | Erase of int * int
  | Before of category
  | After of category

let def e = e.def

type mark_offsets = (int * int) list

type e =
    TSeq of e list * expr * sem
  | TExp of mark_offsets * expr
  | TMatch of mark_offsets

(****)

let print_kind ch k =
  Format.fprintf ch "%s"
    (match k with
       `Shortest -> "short"
     | `Longest  -> "long"
     | `First    -> "first")

let rec print_expr ch e =
  match e.def with
    Cst l ->
      Format.fprintf ch "@[<3>(cst@ %a)@]" Cset.print l;
  | Alt l ->
      Format.fprintf ch "@[<3>(alt";
      List.iter (fun e -> Format.fprintf ch "@ %a" print_expr e) l;
      Format.fprintf ch ")@]"
  | Seq (k, e, e') ->
      Format.fprintf ch "@[<3>(seq %a@ %a@ %a)@]"
        print_kind k print_expr e print_expr e'
  | Eps ->
      Format.fprintf ch "eps"
  | Rep (rk, k, e) ->
      Format.fprintf ch "@[<3>(rep@ %a %a)@]" print_kind k print_expr e
  | Mark i ->
      Format.fprintf ch "@[<3>(mark@ %d)@]" i
  | Erase (b, e) ->
      Format.fprintf ch "@[<3>(erase@ %d %d)@]" b e
  | Before c ->
      Format.fprintf ch "@[<3>(before@ %d)@]" c
  | After c ->
      Format.fprintf ch "@[<3>(after@ %d)@]" c

let print_marks ch l =
  match l with
    [] ->
      ()
  | (a, i) :: r ->
      Format.fprintf ch "%d-%d" a i;
      List.iter (fun (a, i) -> Format.fprintf ch " %d-%d" a i) r

let rec print_state_rec ch e y =
  match e with
    TMatch marks ->
      Format.fprintf ch "@[<2>(Match@ %a)@]" print_marks marks
  | TSeq (l', x, kind) ->
      Format.fprintf ch "@[<2>(Seq@ ";
      print_state_lst ch l' x;
      Format.fprintf ch " %a)@]" print_expr x
  | TExp (marks, {def = Eps}) ->
      Format.fprintf ch "(Exp %d (%a) (eps))" y.id print_marks marks
  | TExp (marks, x) ->
      Format.fprintf ch "(Exp %d (%a) %a)" x.id print_marks marks print_expr x

and print_state_lst ch l y =
  match l with
    [] ->
      Format.fprintf ch "()"
  | e :: rem ->
      print_state_rec ch e y;
      List.iter
        (fun e ->
           Format.fprintf ch " | ";
           print_state_rec ch e y)
        rem

let print_state ch l = print_state_lst ch l { id = 0; def = Eps }

(****)

let rec first f l =
  match l with
    [] ->
      None
  | x :: r ->
      match f x with
        None          -> first f r
      | Some _ as res -> res

(****)

type ids = int ref
let create_ids () = ref 0

let eps_expr = { id = 0; def = Eps }

let mk_expr ids def =
  incr ids;
  { id = !ids; def = def }

let empty ids = mk_expr ids (Alt [])

let cst ids s =
  if s = [] then
    empty ids
  else
    mk_expr ids (Cst s)

let alt ids l =
  match l with
    []  -> empty ids
  | [c] -> c
  | l   -> mk_expr ids (Alt l)

let seq ids kind x y =
  match x.def, y.def with
    Alt [], _                 -> x
  | _, Alt []                 -> y
  | Eps, _                    -> y
  | _, Eps when kind = `First -> x
  | _                         -> mk_expr ids (Seq (kind, x, y))

let eps ids = mk_expr ids Eps

let rep ids kind sem x = mk_expr ids (Rep (kind, sem, x))

let mark ids m = mk_expr ids (Mark m)

let erase ids m m' = mk_expr ids (Erase (m, m'))

let before ids c = mk_expr ids (Before c)

let after ids c = mk_expr ids (After c)

let texp marks x = TExp (marks, x)

let tseq kind x y rem =
  match x with
    []                          -> rem
  | [TExp (marks, {def = Eps})] -> TExp (marks, y) :: rem
  | _                           -> TSeq (x, y, kind) :: rem

(****)

let rec rename ids x =
  match x.def with
    Cst _ | Eps | Mark _ | Erase _ | Before _ | After _ ->
      mk_expr ids x.def
  | Alt l ->
      mk_expr ids (Alt (List.map (rename ids) l))
  | Seq (k, y, z) ->
      mk_expr ids (Seq (k, rename ids y, rename ids z))
  | Rep (g, k, y) ->
      mk_expr ids (Rep (g, k, rename ids y))

(****)

type hash = int
type mark_infos = int array
type status = [`Failed | `Match of mark_infos | `Running]
type state = int * category * e list * status option ref * hash

let dummy_state = (-1, -1, [], ref None, -1)

let hash_combine h accu = accu * 65599 + h

let rec hash_marks l accu =
  match l with
    []          -> accu
  | (a, i) :: r -> hash_marks r (hash_combine a (hash_combine i accu))

let rec hash_e l accu =
  match l with
    [] ->
      accu
  | TSeq (l', e, _) :: r ->
      hash_e r (hash_combine 0x172a1bce (hash_combine e.id (hash_e l' accu)))
  | TExp (marks, e) :: r ->
      hash_e r
        (hash_combine 0x2b4c0d77 (hash_combine e.id (hash_marks marks accu)))
  | TMatch marks :: r ->
      hash_e r (hash_combine 0x1c205ad5 (hash_marks marks accu))

let hash_state idx cat desc =
  hash_e desc (hash_combine idx (hash_combine cat 0)) land 0x3FFFFFFF

let mk_state idx cat desc = (idx, cat, desc, ref None, hash_state idx cat desc)

let create_state cat e = mk_state 0 cat [TExp ([], e)]

let rec equal_e l1 l2 =
  match l1, l2 with
    [], [] ->
      true
  | TSeq (l1', e1, _) :: r1, TSeq (l2', e2, _) :: r2 ->
      e1.id = e2.id && equal_e l1' l2' && equal_e r1 r2
  | TExp (marks1, e1) :: r1, TExp (marks2, e2) :: r2 ->
      e1.id = e2.id && marks1 = marks2 && equal_e r1 r2
  | TMatch marks1 :: r1, TMatch marks2 :: r2 ->
      marks1 = marks2 && equal_e r1 r2
  | _ ->
      false

let equal_state (idx1, cat1, desc1, _, h1) (idx2, cat2, desc2, _, h2) =
  (h1 : int) = h2 && (idx1 : int) = idx2 &&
  (cat1 : int) = cat2 && equal_e desc1 desc2

let compare_state (idx1, cat1, desc1, _, h1) (idx2, cat2, desc2, _, h2) =
  let c = compare (h1 : int) h2 in
  if c <> 0 then c else
  let c = compare (cat1 : int) cat2 in
  if c <> 0 then c else
  compare desc1 desc2

module States =
  Hashtbl.Make
    (struct
       type t = state
       let equal = equal_state
       let hash (_, _, _, _, h) = h
     end)

(**** Find a free index ****)

type working_area = bool array ref

let create_working_area () = ref [| false |]

let index_count w = Array.length !w

let reset_table a = Array.fill a 0 (Array.length a) false

let rec mark_used_indices tbl l =
  List.iter
    (fun x ->
       match x with
         TSeq (l, _, _) ->
           mark_used_indices tbl l
       | TExp (marks, _) ->
           List.iter (fun (_, i) -> if i >= 0 then tbl.(i) <- true) marks
       | TMatch marks ->
           List.iter (fun (_, i) -> if i >= 0 then tbl.(i) <- true) marks)
    l

let rec find_free tbl idx len =
  if idx = len || not tbl.(idx) then idx else find_free tbl (idx + 1) len

let free_index tbl_ref l =
  let tbl = !tbl_ref in
  reset_table tbl;
  mark_used_indices tbl l;
  let len = Array.length tbl in
  let idx = find_free tbl 0 len in
  if idx = len then tbl_ref := Array.make (2 * len) false;
  idx

(**** Computation of the next state ****)

let remove_matches l =
  List.filter (fun x -> match x with TMatch _ -> false | _ -> true) l

let rec split_at_match_rec l' l =
  match l with
    []            -> assert false
  | TMatch _ :: r -> (List.rev l', remove_matches r)
  | x :: r        -> split_at_match_rec (x :: l') r

let split_at_match l = split_at_match_rec [] l

let rec remove_duplicates prev l y =
  match l with
    [] ->
      ([], prev)
  | TMatch _ as x :: r -> (* Truncate after first match *)
      ([x], prev)
  | TSeq (l', x, kind) :: r ->
      let (l'', prev') = remove_duplicates prev l' x in
      let (r', prev'') = remove_duplicates prev' r y in
      (tseq kind l'' x r', prev'')
  | TExp (marks, {def = Eps}) as e :: r ->
      if List.memq y.id prev then
        remove_duplicates prev r y
      else
        let (r', prev') = remove_duplicates (y.id :: prev) r y in
        (e :: r', prev')
  | TExp (marks, x) as e :: r ->
      if List.memq x.id prev then
        remove_duplicates prev r y
      else
        let (r', prev') = remove_duplicates (x.id :: prev) r y in
        (e :: r', prev')

let rec marks_set_idx used idx marks =
  match marks with
    (a, -1) :: rem ->
      used := true;
      (a, idx) :: marks_set_idx used idx rem
  | _ ->
      marks

let rec set_idx used idx l =
  match l with
    [] ->
      []
  | TMatch marks :: r ->
      TMatch (marks_set_idx used idx marks) :: set_idx used idx r
  | TSeq (l', x, kind) :: r ->
      TSeq (set_idx used idx l', x, kind) :: set_idx used idx r
  | TExp (marks, x) :: r ->
      TExp (marks_set_idx used idx marks, x) :: set_idx used idx r

let rec filter_marks b e marks =
  List.filter (fun (i, _) -> i < b || i > e) marks

let rec delta_1 marks c cat' cat x rem =
(*Format.eprintf "%d@." x.id;*)
  match x.def with
    Cst s ->
      if Cset.mem c s then texp marks eps_expr :: rem else rem
  | Alt l ->
      delta_2 marks c cat' cat l rem
  | Seq (kind, y, z) ->
      let y' = delta_1 marks c cat' cat y [] in
      delta_seq c cat' cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
      let y' = delta_1 marks c cat' cat y [] in
      let (y'', marks') =
        match
          first
            (fun x -> match x with TMatch marks -> Some marks | _ -> None) y'
        with
          None        -> (y', marks)
        | Some marks' -> (remove_matches y', marks')
      in
      begin match rep_kind with
        `Greedy     -> tseq kind y'' x (TMatch marks' :: rem)
      | `Non_greedy -> TMatch marks :: tseq kind y'' x rem
      end
  | Eps ->
      TMatch marks :: rem
  | Mark i ->
      TMatch ((i, -1) :: List.remove_assq i marks) :: rem
  | Erase (b, e) ->
      TMatch (filter_marks b e marks) :: rem
  | Before cat'' ->
      if cat land cat'' <> 0 then TMatch marks :: rem else rem
  | After cat'' ->
      if cat' land cat'' <> 0 then TMatch marks :: rem else rem

and delta_2 marks c cat' cat l rem =
  match l with
    []     -> rem
  | y :: r -> delta_1 marks c cat' cat y (delta_2 marks c cat' cat r rem)

and delta_seq c cat' cat kind y z rem =
  match
    first (fun x -> match x with TMatch marks -> Some marks | _ -> None) y
  with
    None ->
      tseq kind y z rem
  | Some marks ->
      match kind with
        `Longest ->
          tseq kind (remove_matches y) z (delta_1 marks c cat' cat z rem)
      | `Shortest ->
          delta_1 marks c cat' cat z (tseq kind (remove_matches y) z rem)
      | `First ->
          let (y', y'') = split_at_match y in
          tseq kind y' z (delta_1 marks c cat' cat z (tseq kind y'' z rem))

let rec delta_3 c cat' cat x rem =
  match x with
    TSeq (y, z, kind) ->
      let y' = delta_4 c cat' cat y [] in
      delta_seq c cat' cat kind y' z rem
  | TExp (marks, e) ->
      delta_1 marks c cat' cat e rem
  | TMatch _ ->
      x :: rem

and delta_4 c cat' cat l rem =
  match l with
    []     -> rem
  | y :: r -> delta_3 c cat' cat y (delta_4 c cat' cat r rem)

let delta tbl_ref cat' char (_, cat, expr, _, _) =
  let (expr', _) =
    remove_duplicates [] (delta_4 char cat cat' expr []) eps_expr in
  let idx = free_index tbl_ref expr' in
  let used = ref false in
  let expr'' = set_idx used idx expr' in
  mk_state idx cat' expr''

(****)

let rec red_tr l =
  match l with
    [] | [_] ->
      l
  | ((s1, st1) as tr1) :: ((s2, st2) as tr2) :: rem ->
      if equal_state st1 st2 then
        red_tr ((Cset.union s1 s2, st1) :: rem)
      else
        tr1 :: red_tr (tr2 :: rem)

let simpl_tr l =
  List.sort
    (fun (s1, _) (s2, _) -> compare s1 s2)
  (red_tr (List.sort (fun (_, st1) (_, st2) -> compare_state st1 st2) l))

(****)

let rec prepend s x l =
  match s, l with
    [], _ ->
      l
  | _, [] ->
      []
  | (c, c') :: r, ([d, d'], x') :: r' when c' < d ->
      prepend r x l
  | (c, c') :: r, ([d, d'], x') :: r' ->
      if c <= d then begin
        if c' < d' then
          ([d, c'], x @ x') :: prepend r x (([c' + 1, d'], x') :: r')
        else
          ([d, d'], x @ x') :: prepend s x r'
      end else begin
        if c > d' then
          ([d, d'], x') :: prepend s x r'
        else
          ([d, c - 1], x') :: prepend s x (([c, d'], x') :: r')
      end
  | _ ->
      assert false

let prepend_deriv d l = List.fold_right (fun (s, x) l -> prepend s x l) d l

let rec restrict s l =
  match l with
    [] ->
      []
  | (s', x') :: rem ->
      let s'' = Cset.inter s s' in
      if s'' = [] then
        restrict s rem
      else
        (s'', x') :: restrict s rem

let rec remove_marks b e rem =
  if b > e then rem else remove_marks b (e - 1) ((e, -2) :: rem)

let rec merge_marks old nw =
  match nw with
    [] ->
      old
  | (i, v) :: rem ->
      let nw' = merge_marks (List.remove_assq i old) rem in
      if v = -2 then
        nw'
      else
        (i, v) :: nw'

let rec prepend_marks_expr m e =
  match e with
    TSeq (l, e', s) -> TSeq (prepend_marks_expr_lst m l, e', s)
  | TExp (m', e')   -> TExp (merge_marks m m', e')
  | TMatch m'       -> TMatch (merge_marks m m')

and prepend_marks_expr_lst m l =
  List.map (prepend_marks_expr m) l

let prepend_marks (m : mark_offsets) l =
  List.map (fun (s, x) -> (s, prepend_marks_expr_lst m x)) l

let rec deriv_1 all_chars categories marks cat x rem =
  match x.def with
    Cst s ->
      prepend s [texp marks eps_expr] rem
  | Alt l ->
      deriv_2 all_chars categories marks cat l rem
  | Seq (kind, y, z) ->
      let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
      deriv_seq all_chars categories cat kind y' z rem
  | Rep (rep_kind, kind, y) ->
      let y' = deriv_1 all_chars categories marks cat y [(all_chars, [])] in
      List.fold_right
        (fun (s, z) rem ->
           let (z', marks') =
             match
               first
                 (fun z -> match z with TMatch marks -> Some marks | _ -> None)
                 z
             with
               None        -> (z, marks)
             | Some marks' -> (remove_matches z, marks')
           in
           prepend s
             (match rep_kind with
                `Greedy     -> tseq kind z' x [TMatch marks']
              | `Non_greedy -> TMatch marks :: tseq kind z' x [])
             rem)
        y' rem
  | Eps ->
      prepend all_chars [TMatch marks] rem
  | Mark i ->
      prepend all_chars [TMatch ((i, -1) :: List.remove_assq i marks)] rem
  | Erase (b, e) ->
      prepend all_chars
        [TMatch (remove_marks b e (filter_marks b e marks))] rem
  | Before cat' ->
      prepend (List.assq cat' categories) [TMatch marks] rem
  | After cat' ->
      if cat land cat' <> 0 then prepend all_chars [TMatch marks] rem else rem

and deriv_2 all_chars categories marks cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_1 all_chars categories marks cat y
                (deriv_2 all_chars categories marks cat r rem)

and deriv_seq all_chars categories cat kind y z rem =
  if
    List.exists
      (fun (s, xl) ->
         List.exists (fun x -> match x with TMatch _ -> true | _ -> false) xl)
      y
  then
    let z' = deriv_1 all_chars categories [] cat z [(all_chars, [])] in
    List.fold_right
      (fun (s, y) rem ->
         match
           first (fun x -> match x with TMatch marks -> Some marks | _ -> None)
             y
         with
           None ->
             prepend s (tseq kind y z []) rem
         | Some marks ->
             let z'' = prepend_marks marks z' in
             match kind with
               `Longest ->
                 prepend s (tseq kind (remove_matches y) z []) (
                 prepend_deriv (restrict s z'') rem)
             | `Shortest ->
                 prepend_deriv (restrict s z'') (
                 prepend s (tseq kind (remove_matches y) z []) rem)
             | `First ->
                 let (y', y'') = split_at_match y in
                 prepend s (tseq kind y' z []) (
                 prepend_deriv (restrict s z'') (
                 prepend s (tseq kind y'' z []) rem)))
      y rem
  else
    List.fold_right
      (fun (s, xl) rem -> prepend s (tseq kind xl z []) rem) y rem

let rec deriv_3 all_chars categories cat x rem =
  match x with
    TSeq (y, z, kind) ->
      let y' = deriv_4 all_chars categories cat y [(all_chars, [])] in
      deriv_seq all_chars categories cat kind y' z rem
  | TExp (marks, e) ->
      deriv_1 all_chars categories marks cat e rem
  | TMatch _ ->
      prepend all_chars [x] rem

and deriv_4 all_chars categories cat l rem =
  match l with
    []     -> rem
  | y :: r -> deriv_3 all_chars categories cat y
                (deriv_4 all_chars categories cat r rem)

let deriv tbl_ref all_chars categories (_, cat, expr, _, _) =
  let der = deriv_4 all_chars categories cat expr [(all_chars, [])] in
  simpl_tr
    (List.fold_right
       (fun (s, expr) rem ->
          let (expr', _) = remove_duplicates [] expr eps_expr in
(*
Format.eprintf "@[<3>@[%a@]: %a / %a@]@." Cset.print s print_state expr print_state expr';
*)
          let idx = free_index tbl_ref expr' in
          let used = ref false in
          let expr'' = set_idx used idx expr' in
          List.fold_right
            (fun (cat', s') rem ->
               let s'' = Cset.inter s s' in
               if s'' = [] then rem else
               (s'', mk_state idx cat' expr'') :: rem)
            categories rem)
       der [])

(****)

let flatten_match m =
  let ma = List.fold_left (fun ma (i, _) -> max ma i) (-1) m in
  let res = Array.create (ma + 1) (-1) in
  List.iter (fun (i, v) -> res.(i) <- v) m;
  res

let status (_, _, desc, status, _) =
  match !status with
    Some st ->
      st
  | None ->
      let st =
        match desc with
          []            -> `Failed
        | TMatch m :: _ -> `Match (flatten_match m)
        | _             -> `Running
      in
      status := Some st;
      st
