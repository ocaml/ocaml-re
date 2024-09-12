open Import

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Ids = struct
  type t = int ref

  let create () = ref 0

  let next t =
    incr t;
    !t
  ;;
end

module Sem = struct
  type t =
    [ `Longest
    | `Shortest
    | `First
    ]

  let equal = Poly.equal

  let pp ch k =
    Format.pp_print_string
      ch
      (match k with
       | `Shortest -> "short"
       | `Longest -> "long"
       | `First -> "first")
  ;;
end

module Rep_kind = struct
  type t =
    [ `Greedy
    | `Non_greedy
    ]

  let pp fmt = function
    | `Greedy -> Format.pp_print_string fmt "Greedy"
    | `Non_greedy -> Format.pp_print_string fmt "Non_greedy"
  ;;
end

module Mark : sig
  type t = private int

  val pp : t Fmt.t
  val start : t
  val prev : t -> t
  val next : t -> t
  val next2 : t -> t
  val group_count : t -> int
end = struct
  type t = int

  let pp = Format.pp_print_int
  let start = 0
  let prev x = pred x
  let next x = succ x
  let next2 x = x + 2
  let group_count x = x / 2
end

type idx = int

type expr =
  { id : int
  ; def : def
  }

and def =
  | Cst of Cset.t
  | Alt of expr list
  | Seq of Sem.t * expr * expr
  | Eps
  | Rep of Rep_kind.t * Sem.t * expr
  | Mark of Mark.t
  | Erase of Mark.t * Mark.t
  | Before of Category.t
  | After of Category.t
  | Pmark of Pmark.t

let hash_combine h accu = (accu * 65599) + h

module Marks = struct
  type t =
    { marks : (int * int) list
    ; pmarks : Pmark.Set.t
    }

  let equal { marks; pmarks } t =
    List.equal ~eq:(fun (x, y) (x', y') -> Int.equal x x' && Int.equal y y') marks t.marks
    && Pmark.Set.equal pmarks t.pmarks
  ;;

  let empty = { marks = []; pmarks = Pmark.Set.empty }

  let merge =
    let rec merge_marks_offset old = function
      | [] -> old
      | (i, v) :: rem ->
        let nw' = merge_marks_offset (List.remove_assq i old) rem in
        if v = -2 then nw' else (i, v) :: nw'
    in
    fun old nw ->
      { marks = merge_marks_offset old.marks nw.marks
      ; pmarks = Pmark.Set.union old.pmarks nw.pmarks
      }
  ;;

  let rec hash_marks_offset l accu =
    match l with
    | [] -> accu
    | (a, i) :: r -> hash_marks_offset r (hash_combine a (hash_combine i accu))
  ;;

  let hash m accu = hash_marks_offset m.marks (hash_combine (Hashtbl.hash m.pmarks) accu)

  let marks_set_idx =
    let rec marks_set_idx idx = function
      | (a, -1) :: rem -> (a, idx) :: marks_set_idx idx rem
      | marks -> marks
    in
    fun marks idx -> { marks with marks = marks_set_idx idx marks.marks }
  ;;

  let rec remove_marks b e rem =
    if b > e then rem else remove_marks b (e - 1) ((e, -2) :: rem)
  ;;

  let remove_marks (b : Mark.t) (e : Mark.t) rem = remove_marks (b :> int) (e :> int) rem

  let filter t (b : Mark.t) (e : Mark.t) =
    { t with
      marks = List.filter ~f:(fun (i, _) -> i < (b :> int) || i > (e :> int)) t.marks
    }
  ;;

  let erase t b e = { t with marks = remove_marks b e (filter t b e).marks }

  let set_mark t (i : Mark.t) =
    { t with marks = ((i :> int), -1) :: List.remove_assq (i :> int) t.marks }
  ;;

  let set_pmark t i = { t with pmarks = Pmark.Set.add i t.pmarks }

  let pp_marks ch t =
    match t.marks with
    | [] -> ()
    | (a, i) :: r ->
      Format.fprintf ch "%d-%d" a i;
      List.iter ~f:(fun (a, i) -> Format.fprintf ch " %d-%d" a i) r
  ;;
end

(****)

let rec pp ch e =
  let open Fmt in
  match e.def with
  | Cst l -> sexp ch "cst" Cset.pp l
  | Alt l -> sexp ch "alt" (list pp) l
  | Seq (k, e, e') -> sexp ch "seq" (triple Sem.pp pp pp) (k, e, e')
  | Eps -> str ch "eps"
  | Rep (_rk, k, e) -> sexp ch "rep" (pair Sem.pp pp) (k, e)
  | Mark i -> sexp ch "mark" int (i :> int)
  | Pmark i -> sexp ch "pmark" int (i :> int)
  | Erase (b, e) -> sexp ch "erase" (pair Mark.pp Mark.pp) (b, e)
  | Before c -> sexp ch "before" Category.pp c
  | After c -> sexp ch "after" Category.pp c
;;

(****)
let eps_expr = { id = 0; def = Eps }
let mk_expr ids def = { id = Ids.next ids; def }
let empty ids = mk_expr ids (Alt [])
let cst ids s = if Cset.is_empty s then empty ids else mk_expr ids (Cst s)

let alt ids = function
  | [] -> empty ids
  | [ c ] -> c
  | l -> mk_expr ids (Alt l)
;;

let seq ids (kind : Sem.t) x y =
  match x.def, y.def with
  | Alt [], _ -> x
  | _, Alt [] -> y
  | Eps, _ -> y
  | _, Eps when Sem.equal kind `First -> x
  | _ -> mk_expr ids (Seq (kind, x, y))
;;

let is_eps expr =
  match expr.def with
  | Eps -> true
  | _ -> false
;;

let eps ids = mk_expr ids Eps
let rep ids kind sem x = mk_expr ids (Rep (kind, sem, x))
let mark ids m = mk_expr ids (Mark m)
let pmark ids i = mk_expr ids (Pmark i)
let erase ids m m' = mk_expr ids (Erase (m, m'))
let before ids c = mk_expr ids (Before c)
let after ids c = mk_expr ids (After c)

(****)

let rec rename ids x =
  match x.def with
  | Cst _ | Eps | Mark _ | Pmark _ | Erase _ | Before _ | After _ -> mk_expr ids x.def
  | Alt l -> mk_expr ids (Alt (List.map ~f:(rename ids) l))
  | Seq (k, y, z) -> mk_expr ids (Seq (k, rename ids y, rename ids z))
  | Rep (g, k, y) -> mk_expr ids (Rep (g, k, rename ids y))
;;

(****)

type status =
  | Failed
  | Match of Mark_infos.t * Pmark.Set.t
  | Running

module E = struct
  type t =
    | TSeq of Sem.t * t list * expr
    | TExp of Marks.t * expr
    | TMatch of Marks.t

  let is_tmatch = function
    | TMatch _ -> true
    | TSeq _ | TExp _ -> false
  ;;

  let rec equal l1 l2 =
    match l1, l2 with
    | [], [] -> true
    | TSeq (_, l1', e1) :: r1, TSeq (_, l2', e2) :: r2 ->
      e1.id = e2.id && equal l1' l2' && equal r1 r2
    | TExp (marks1, e1) :: r1, TExp (marks2, e2) :: r2 ->
      e1.id = e2.id && Marks.equal marks1 marks2 && equal r1 r2
    | TMatch marks1 :: r1, TMatch marks2 :: r2 -> Marks.equal marks1 marks2 && equal r1 r2
    | _ -> false
  ;;

  let rec hash l accu =
    match l with
    | [] -> accu
    | TSeq (_, l', e) :: r ->
      hash r (hash_combine 0x172a1bce (hash_combine e.id (hash l' accu)))
    | TExp (marks, e) :: r ->
      hash r (hash_combine 0x2b4c0d77 (hash_combine e.id (Marks.hash marks accu)))
    | TMatch marks :: r -> hash r (hash_combine 0x1c205ad5 (Marks.hash marks accu))
  ;;

  let texp marks x = TExp (marks, x)

  let tseq kind x y rem =
    match x with
    | [] -> rem
    | [ TExp (marks, { def = Eps; _ }) ] -> TExp (marks, y) :: rem
    | _ -> TSeq (kind, x, y) :: rem
  ;;

  let rec print_state_rec ch e y =
    match e with
    | TMatch marks -> Format.fprintf ch "@[<2>(Match@ %a)@]" Marks.pp_marks marks
    | TSeq (_kind, l', x) ->
      Format.fprintf ch "@[<2>(Seq@ ";
      print_state_lst ch l' x;
      Format.fprintf ch "@ %a)@]" pp x
    | TExp (marks, { def = Eps; _ }) ->
      Format.fprintf ch "@[<2>(Exp@ %d@ (%a)@ (eps))@]" y.id Marks.pp_marks marks
    | TExp (marks, x) ->
      Format.fprintf ch "@[<2>(Exp@ %d@ (%a)@ %a)@]" x.id Marks.pp_marks marks pp x

  and print_state_lst ch l y =
    match l with
    | [] -> Format.fprintf ch "()"
    | e :: rem ->
      print_state_rec ch e y;
      List.iter rem ~f:(fun e ->
        Format.fprintf ch "@ | ";
        print_state_rec ch e y)
  ;;

  let _pp ch t = print_state_lst ch [ t ] { id = 0; def = Eps }
end

module Desc = struct
  type t = E.t list

  open E

  let rec first_match = function
    | [] -> None
    | TMatch marks :: _ -> Some marks
    | _ :: r -> first_match r
  ;;

  let remove_matches =
    List.filter ~f:(function
      | TMatch _ -> false
      | _ -> true)
  ;;

  let split_at_match =
    let rec split_at_match_rec l = function
      | [] -> assert false
      | TMatch _ :: r -> List.rev l, remove_matches r
      | x :: r -> split_at_match_rec (x :: l) r
    in
    fun l -> split_at_match_rec [] l
  ;;

  let exists_tmatch = List.exists ~f:is_tmatch

  let rec set_idx idx = function
    | [] -> []
    | TMatch marks :: r -> TMatch (Marks.marks_set_idx marks idx) :: set_idx idx r
    | TSeq (kind, l, x) :: r -> TSeq (kind, set_idx idx l, x) :: set_idx idx r
    | TExp (marks, x) :: r -> TExp (Marks.marks_set_idx marks idx, x) :: set_idx idx r
  ;;
end

module State = struct
  type t =
    { idx : idx
    ; category : Category.t
    ; desc : Desc.t
    ; mutable status : status option
    ; hash : int
    }

  let[@inline] idx t = t.idx
  let dummy = { idx = -1; category = Category.dummy; desc = []; status = None; hash = -1 }

  let hash idx cat desc =
    E.hash desc (hash_combine idx (hash_combine (Category.to_int cat) 0)) land 0x3FFFFFFF
  ;;

  let mk idx cat desc =
    { idx; category = cat; desc; status = None; hash = hash idx cat desc }
  ;;

  let create cat e = mk 0 cat [ TExp (Marks.empty, e) ]

  let equal { idx; category; desc; status = _; hash } t =
    Int.equal hash t.hash
    && Int.equal idx t.idx
    && Category.equal category t.category
    && E.equal desc t.desc
  ;;

  let compare { hash; category; desc; status = _; idx } t =
    match Int.compare idx t.idx with
    | 0 ->
      (match Int.compare hash t.hash with
       | 0 ->
         (match Category.compare category t.category with
          | 0 -> Poly.compare desc t.desc
          | x -> x)
       | x -> x)
    | x -> x
  ;;

  let status s =
    match s.status with
    | Some st -> st
    | None ->
      let st =
        match s.desc with
        | [] -> Failed
        | TMatch m :: _ -> Match (Mark_infos.make m.marks, m.pmarks)
        | _ -> Running
      in
      s.status <- Some st;
      st
  ;;

  module Table = Hashtbl.Make (struct
      type nonrec t = t

      let equal = equal
      let hash t = t.hash
    end)
end

(**** Find a free index ****)

module Working_area = struct
  type t =
    { mutable ids : Bit_vector.t
    ; seen : Hash_set.t
    }

  let create () = { ids = Bit_vector.create_zero 1; seen = Hash_set.create () }
  let index_count w = Bit_vector.length w.ids

  let rec mark_used_indices tbl =
    List.iter ~f:(fun (e : E.t) ->
      match e with
      | TSeq (_, l, _) -> mark_used_indices tbl l
      | TExp (marks, _) | TMatch marks ->
        List.iter marks.marks ~f:(fun (_, i) -> if i >= 0 then Bit_vector.set tbl i true))
  ;;

  let rec find_free tbl idx len =
    if idx = len || not (Bit_vector.get tbl idx) then idx else find_free tbl (idx + 1) len
  ;;

  let free_index t l =
    Bit_vector.reset_zero t.ids;
    mark_used_indices t.ids l;
    let len = Bit_vector.length t.ids in
    let idx = find_free t.ids 0 len in
    if idx = len then t.ids <- Bit_vector.create_zero (2 * len);
    idx
  ;;
end

(**** Computation of the next state ****)

let remove_duplicates =
  let rec loop seen (l : Desc.t) y =
    match l with
    | [] -> []
    | (TMatch _ as x) :: _ ->
      (* Truncate after first match *)
      [ x ]
    | TSeq (kind, l, x) :: r ->
      let l = loop seen l x in
      let r = loop seen r y in
      E.tseq kind l x r
    | (TExp (_marks, { def = Eps; _ }) as e) :: r ->
      if Hash_set.mem seen y.id
      then loop seen r y
      else (
        Hash_set.add seen y.id;
        e :: loop seen r y)
    | (TExp (_marks, x) as e) :: r ->
      if Hash_set.mem seen x.id
      then loop seen r y
      else (
        Hash_set.add seen x.id;
        e :: loop seen r y)
  in
  fun seen l y ->
    Hash_set.clear seen;
    loop seen l y
;;

type ctx =
  { c : Cset.c
  ; prev_cat : Category.t
  ; next_cat : Category.t
  }

let rec delta_expr ({ c; _ } as ctx) marks x rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
  | Cst s -> if Cset.mem c s then E.texp marks eps_expr :: rem else rem
  | Alt l -> delta_alt ctx marks l rem
  | Seq (kind, y, z) ->
    let y = delta_expr ctx marks y [] in
    delta_seq ctx kind y z rem
  | Rep (rep_kind, kind, y) ->
    let y, marks' =
      let y = delta_expr ctx marks y [] in
      match Desc.first_match y with
      | None -> y, marks
      | Some marks -> Desc.remove_matches y, marks
    in
    (match rep_kind with
     | `Greedy -> E.tseq kind y x (TMatch marks' :: rem)
     | `Non_greedy -> TMatch marks :: E.tseq kind y x rem)
  | Eps -> TMatch marks :: rem
  | Mark i -> TMatch (Marks.set_mark marks i) :: rem
  | Pmark i -> TMatch (Marks.set_pmark marks i) :: rem
  | Erase (b, e) -> TMatch (Marks.filter marks b e) :: rem
  | Before cat -> if Category.intersect ctx.next_cat cat then TMatch marks :: rem else rem
  | After cat -> if Category.intersect ctx.prev_cat cat then TMatch marks :: rem else rem

and delta_alt ctx marks l rem =
  match l with
  | [] -> rem
  | y :: r -> delta_expr ctx marks y (delta_alt ctx marks r rem)

and delta_seq ctx (kind : Sem.t) y z rem =
  match Desc.first_match y with
  | None -> E.tseq kind y z rem
  | Some marks ->
    (match kind with
     | `Longest -> E.tseq kind (Desc.remove_matches y) z (delta_expr ctx marks z rem)
     | `Shortest -> delta_expr ctx marks z (E.tseq kind (Desc.remove_matches y) z rem)
     | `First ->
       let y, y' = Desc.split_at_match y in
       E.tseq kind y z (delta_expr ctx marks z (E.tseq kind y' z rem)))
;;

let rec delta_e ctx marks (x : E.t) rem =
  match x with
  | TSeq (kind, y, z) ->
    let y = delta_desc ctx marks y [] in
    delta_seq ctx kind y z rem
  | TExp (marks, e) -> delta_expr ctx marks e rem
  | TMatch _ -> x :: rem

and delta_desc ctx marks l rem =
  match l with
  | [] -> rem
  | y :: r -> delta_e ctx marks y (delta_desc ctx marks r rem)
;;

let delta (tbl_ref : Working_area.t) next_cat char (st : State.t) =
  let expr =
    let prev_cat = st.category in
    let ctx = { c = char; next_cat; prev_cat } in
    remove_duplicates tbl_ref.seen (delta_desc ctx Marks.empty st.desc []) eps_expr
  in
  let idx = Working_area.free_index tbl_ref expr in
  let expr = Desc.set_idx idx expr in
  State.mk idx next_cat expr
;;

(****)

let rec red_tr = function
  | ([] | [ _ ]) as l -> l
  | ((s1, st1) as tr1) :: ((s2, st2) as tr2) :: rem ->
    if State.equal st1 st2
    then red_tr ((Cset.union s1 s2, st1) :: rem)
    else tr1 :: red_tr (tr2 :: rem)
;;

let simpl_tr l =
  List.sort ~cmp:(fun (_, st1) (_, st2) -> State.compare st1 st2) l
  |> red_tr
  |> List.sort ~cmp:(fun (s1, _) (s2, _) -> Cset.compare s1 s2)
;;

(****)

let prepend_deriv init = List.fold_right ~init ~f:(fun (s, x) l -> Cset.prepend s x l)

let rec restrict s = function
  | [] -> []
  | (s', x) :: rem ->
    let s' = Cset.inter s s' in
    if Cset.is_empty s' then restrict s rem else (s', x) :: restrict s rem
;;

let prepend_marks =
  let rec prepend_marks_expr m (e : E.t) : E.t =
    match e with
    | TSeq (s, l, e) -> TSeq (s, prepend_marks_expr_lst m l, e)
    | TExp (m', e) -> TExp (Marks.merge m m', e)
    | TMatch m' -> TMatch (Marks.merge m m')
  and prepend_marks_expr_lst m l = List.map ~f:(prepend_marks_expr m) l in
  fun m -> List.map ~f:(fun (s, x) -> s, prepend_marks_expr_lst m x)
;;

let rec deriv_expr all_chars categories marks cat x rem =
  match x.def with
  | Cst s -> Cset.prepend s [ E.texp marks eps_expr ] rem
  | Alt l -> deriv_alt all_chars categories marks cat l rem
  | Seq (kind, y, z) ->
    let y = deriv_expr all_chars categories marks cat y [ all_chars, [] ] in
    deriv_seq all_chars categories cat kind y z rem
  | Rep (rep_kind, kind, y) ->
    let y = deriv_expr all_chars categories marks cat y [ all_chars, [] ] in
    List.fold_right ~init:rem y ~f:(fun (s, z) rem ->
      let z', marks' =
        match Desc.first_match z with
        | None -> z, marks
        | Some marks' -> Desc.remove_matches z, marks'
      in
      Cset.prepend
        s
        (match rep_kind with
         | `Greedy -> E.tseq kind z' x [ TMatch marks' ]
         | `Non_greedy -> TMatch marks :: E.tseq kind z' x [])
        rem)
  | Eps -> Cset.prepend all_chars [ E.TMatch marks ] rem
  | Mark i -> Cset.prepend all_chars [ E.TMatch (Marks.set_mark marks i) ] rem
  | Pmark _ -> Cset.prepend all_chars [ E.TMatch marks ] rem
  | Erase (b, e) -> Cset.prepend all_chars [ E.TMatch (Marks.erase marks b e) ] rem
  | Before cat -> Cset.prepend (List.assq cat categories) [ E.TMatch marks ] rem
  | After cat' ->
    if Category.intersect cat cat'
    then Cset.prepend all_chars [ E.TMatch marks ] rem
    else rem

and deriv_alt all_chars categories marks cat l rem =
  match l with
  | [] -> rem
  | y :: r ->
    deriv_expr
      all_chars
      categories
      marks
      cat
      y
      (deriv_alt all_chars categories marks cat r rem)

and deriv_seq all_chars categories cat kind y z rem =
  if List.exists y ~f:(fun (_s, xl) -> Desc.exists_tmatch xl)
  then (
    let z' = deriv_expr all_chars categories Marks.empty cat z [ all_chars, [] ] in
    List.fold_right ~init:rem y ~f:(fun (s, y) rem ->
      match Desc.first_match y with
      | None -> Cset.prepend s (E.tseq kind y z []) rem
      | Some marks ->
        let z'' = prepend_marks marks z' |> restrict s in
        (match kind with
         | `Longest ->
           Cset.prepend
             s
             (E.tseq kind (Desc.remove_matches y) z [])
             (prepend_deriv z'' rem)
         | `Shortest ->
           prepend_deriv
             z''
             (Cset.prepend s (E.tseq kind (Desc.remove_matches y) z []) rem)
         | `First ->
           let y', y'' = Desc.split_at_match y in
           Cset.prepend
             s
             (E.tseq kind y' z [])
             (prepend_deriv z'' (Cset.prepend s (E.tseq kind y'' z []) rem)))))
  else
    List.fold_right y ~init:rem ~f:(fun (s, xl) rem ->
      Cset.prepend s (E.tseq kind xl z []) rem)
;;

let rec deriv_e all_chars categories cat (x : E.t) rem =
  match x with
  | TSeq (kind, y, z) ->
    let y = deriv_desc all_chars categories cat y [ all_chars, [] ] in
    deriv_seq all_chars categories cat kind y z rem
  | TExp (marks, e) -> deriv_expr all_chars categories marks cat e rem
  | TMatch _ -> Cset.prepend all_chars [ x ] rem

and deriv_desc all_chars categories cat l rem =
  match l with
  | [] -> rem
  | y :: r ->
    deriv_e all_chars categories cat y (deriv_desc all_chars categories cat r rem)
;;

let deriv (tbl_ref : Working_area.t) all_chars categories (st : State.t) =
  deriv_desc all_chars categories st.category st.desc [ all_chars, [] ]
  |> List.fold_right ~init:[] ~f:(fun (s, expr) rem ->
    let expr' = remove_duplicates tbl_ref.seen expr eps_expr in
    (*
       Format.eprintf "@[<3>@[%a@]: %a / %a@]@." Cset.print s print_state expr print_state expr';
    *)
    let idx = Working_area.free_index tbl_ref expr' in
    let expr'' = Desc.set_idx idx expr' in
    List.fold_right categories ~init:rem ~f:(fun (cat', s') rem ->
      let s'' = Cset.inter s s' in
      if Cset.is_empty s'' then rem else (s'', State.mk idx cat' expr'') :: rem))
  |> simpl_tr
;;

(****)
