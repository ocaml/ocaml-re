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

let hash_combine h accu = (accu * 65599) + h

module Ids : sig
  module Id : sig
    type t

    val equal : t -> t -> bool
    val zero : t
    val hash : t -> int
    val pp : t Fmt.t

    module Hash_set : sig
      type id := t
      type t

      val create : unit -> t
      val mem : t -> id -> bool
      val add : t -> id -> unit
      val clear : t -> unit
    end
  end

  type t

  val create : unit -> t
  val next : t -> Id.t
end = struct
  module Id = struct
    type t = int

    module Hash_set = Hash_set

    let equal = Int.equal
    let zero = 0
    let hash x = x
    let pp = Fmt.int
  end

  type t = int ref

  let create () = ref 0

  let next t =
    incr t;
    !t
  ;;
end

module Id = Ids.Id

module Sem = struct
  type t =
    [ `Longest
    | `Shortest
    | `First
    ]

  let to_string = function
    | `Shortest -> "short"
    | `Longest -> "long"
    | `First -> "first"
  ;;

  let to_dyn t = Dyn.enum (to_string t)
  let equal = Poly.equal
  let pp ch k = Format.pp_print_string ch (to_string k)
end

module Rep_kind = struct
  type t =
    [ `Greedy
    | `Non_greedy
    ]

  let to_string = function
    | `Greedy -> "Greedy"
    | `Non_greedy -> "Non_greedy"
  ;;

  let to_dyn t = Dyn.enum (to_string t)
  let pp fmt t = Format.pp_print_string fmt (to_string t)
end

module Mark : sig
  type t = private int

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val start : t
  val prev : t -> t
  val next : t -> t
  val next2 : t -> t
  val group_count : t -> int
  val outside_range : t -> start_inclusive:t -> stop_inclusive:t -> bool
end = struct
  type t = int

  let equal = Int.equal
  let compare = Int.compare
  let pp = Format.pp_print_int
  let to_dyn = Dyn.int
  let start = 0
  let prev x = pred x
  let next x = succ x
  let next2 x = x + 2
  let group_count x = x / 2

  let outside_range t ~start_inclusive ~stop_inclusive =
    t < start_inclusive || t > stop_inclusive
  ;;
end

module Idx : sig
  type t = private int

  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val to_int : t -> int
  val unknown : t
  val initial : t
  val used : t -> bool
  val make : int -> t
  val equal : t -> t -> bool
end = struct
  type t = int

  let to_dyn = Dyn.int
  let to_int x = x
  let pp = Format.pp_print_int
  let used t = t >= 0
  let make x = x
  let equal = Int.equal
  let unknown = -1
  let initial = 0
end

module Expr = struct
  type t =
    { id : Id.t
    ; def : def
    }

  and def =
    | Cst of Cset.t
    | Alt of t list
    | Seq of Sem.t * t * t
    | Eps
    | Rep of Rep_kind.t * Sem.t * t
    | Mark of Mark.t
    | Erase of Mark.t * Mark.t
    | Before of Category.t
    | After of Category.t
    | Pmark of Pmark.t

  let wrap_sem sem sem' v =
    let open Dyn in
    let name = Sem.to_string sem' in
    match sem with
    | Some sem when Sem.equal sem sem' -> v
    | None | Some _ ->
      (match v with
       | List v -> variant name v
       | _ -> variant name [ v ])
  ;;

  let rec seq_as_list sem = function
    | Eps -> []
    | Cst cs -> [ Cst cs ]
    | Seq (sem', x, y) ->
      if Sem.equal sem sem'
      then x.def :: seq_as_list sem y.def
      else raise_notrace Not_found
    | _ -> raise_notrace Not_found
  ;;

  let seq_as_list sem t =
    match seq_as_list sem t with
    | exception Not_found -> None
    | s -> Some s
  ;;

  let rec dyn_of_def sem =
    let open Dyn in
    function
    | Cst cset -> Cset.to_dyn cset
    | Alt alt -> variant "Alt" (List.map ~f:(to_dyn sem) alt)
    | Seq (sem', x, y) ->
      let to_dyn = to_dyn (Some sem') in
      let x =
        match seq_as_list sem' y.def with
        | None -> variant "Seq" [ to_dyn x; to_dyn y ]
        | Some y -> variant "Seq" (to_dyn x :: List.map y ~f:(dyn_of_def sem))
      in
      wrap_sem sem sem' x
    | Eps -> Enum "Eps"
    | Rep (_, sem', t) -> wrap_sem sem sem' (variant "Rep" [ to_dyn (Some sem') t ])
    | Mark m -> variant "Mark" [ Mark.to_dyn m ]
    | Pmark m -> variant "Pmark" [ Pmark.to_dyn m ]
    | Erase (x, y) -> variant "Erase" [ Mark.to_dyn x; Mark.to_dyn y ]
    | Before c -> variant "Before" [ Category.to_dyn c ]
    | After c -> variant "After" [ Category.to_dyn c ]

  and to_dyn sem { id = _; def } = dyn_of_def sem def

  let rec pp_with_sem sem ch e =
    let open Fmt in
    match e.def with
    | Cst l -> sexp ch "cst" Cset.pp l
    | Alt l -> sexp ch "alt" (list (pp_with_sem sem)) l
    | Seq (k, e, e') ->
      sexp ch "seq" (triple Sem.pp (pp_with_sem sem) (pp_with_sem sem)) (k, e, e')
    | Eps -> str ch "eps"
    | Rep (_rk, k, e) -> sexp ch "rep" (pair Sem.pp (pp_with_sem (Some k))) (k, e)
    | Mark i -> sexp ch "mark" Mark.pp i
    | Pmark i -> sexp ch "pmark" Pmark.pp i
    | Erase (b, e) -> sexp ch "erase" (pair Mark.pp Mark.pp) (b, e)
    | Before c -> sexp ch "before" Category.pp c
    | After c -> sexp ch "after" Category.pp c
  ;;

  let pp = pp_with_sem None
  let eps_expr = { id = Id.zero; def = Eps }
  let mk ids def = { id = Ids.next ids; def }
  let empty ids = mk ids (Alt [])
  let cst ids s = if Cset.is_empty s then empty ids else mk ids (Cst s)
  let eps ids = mk ids Eps
  let rep ids kind sem x = mk ids (Rep (kind, sem, x))
  let mark ids m = mk ids (Mark m)
  let pmark ids i = mk ids (Pmark i)
  let erase ids m m' = mk ids (Erase (m, m'))
  let before ids c = mk ids (Before c)
  let after ids c = mk ids (After c)

  let alt ids = function
    | [] -> empty ids
    | [ c ] -> c
    | l -> mk ids (Alt l)
  ;;

  let seq ids (kind : Sem.t) x y =
    match x.def, y.def with
    | Alt [], _ -> x
    | _, Alt [] -> y
    | Eps, _ -> y
    | _, Eps when Sem.equal kind `First -> x
    | _ -> mk ids (Seq (kind, x, y))
  ;;

  let is_eps expr =
    match expr.def with
    | Eps -> true
    | _ -> false
  ;;

  let rec rename ids x =
    match x.def with
    | Cst _ | Eps | Mark _ | Pmark _ | Erase _ | Before _ | After _ -> mk ids x.def
    | Alt l -> mk ids (Alt (List.map ~f:(rename ids) l))
    | Seq (k, y, z) -> mk ids (Seq (k, rename ids y, rename ids z))
    | Rep (g, k, y) -> mk ids (Rep (g, k, rename ids y))
  ;;
end

type expr = Expr.t

include Expr

module Marks = struct
  type t =
    { marks : (Mark.t * Idx.t) list
    ; pmarks : Pmark.Set.t
    }

  let to_dyn { marks; pmarks } : Dyn.t =
    let open Dyn in
    record
      [ ( "marks"
        , List.map marks ~f:(fun (m, idx) -> pair (Mark.to_dyn m) (Idx.to_dyn idx))
          |> list )
      ; "pmarks", Pmark.Set.to_list pmarks |> List.map ~f:Pmark.to_dyn |> list
      ]
  ;;

  let equal { marks; pmarks } t =
    List.equal
      ~eq:(fun (x, y) (x', y') -> Mark.equal x x' && Idx.equal y y')
      marks
      t.marks
    && Pmark.Set.equal pmarks t.pmarks
  ;;

  let empty = { marks = []; pmarks = Pmark.Set.empty }

  let hash_marks_offset =
    let f acc ((a : Mark.t), (i : Idx.t)) =
      hash_combine (a :> int) (hash_combine (i :> int) acc)
    in
    fun l init -> List.fold_left l ~init ~f
  ;;

  let hash m accu = hash_marks_offset m.marks (hash_combine (Hashtbl.hash m.pmarks) accu)

  let marks_set_idx =
    let rec marks_set_idx idx marks =
      match marks with
      | [] -> []
      | (a, idx') :: rem ->
        if Idx.equal idx' Idx.unknown then (a, idx) :: marks_set_idx idx rem else marks
    in
    fun marks idx -> { marks with marks = marks_set_idx idx marks.marks }
  ;;

  let filter t (b : Mark.t) (e : Mark.t) =
    { t with
      marks =
        List.filter t.marks ~f:(fun ((i : Mark.t), _) ->
          Mark.outside_range i ~start_inclusive:b ~stop_inclusive:e)
    }
  ;;

  let set_mark t (i : Mark.t) =
    { t with marks = (i, Idx.unknown) :: List.remove_assq i t.marks }
  ;;

  let set_pmark t i = { t with pmarks = Pmark.Set.add i t.pmarks }

  let pp fmt { marks; pmarks } =
    Format.pp_open_box fmt 1;
    (match marks with
     | [] -> ()
     | _ :: _ ->
       Format.fprintf
         fmt
         "@[<2>marks@ %a@]"
         (Format.pp_print_list (fun fmt (a, i) ->
            Format.fprintf fmt "%a-%a" Mark.pp a Idx.pp i))
         marks);
    (match Pmark.Set.to_list pmarks with
     | [] -> ()
     | pmarks ->
       Format.fprintf fmt "@[<2>pmarks %a@]" (Format.pp_print_list Pmark.pp) pmarks);
    Format.pp_close_box fmt ()
  ;;
end

module Status = struct
  type t =
    | Failed
    | Match of Mark_infos.t * Pmark.Set.t
    | Running
end

module Desc : sig
  type t

  val pp : t Fmt.t

  module E : sig
    type nonrec t = private
      | TSeq of Sem.t * t * Expr.t
      | TExp of Marks.t * Expr.t
      | TMatch of Marks.t
  end

  val to_dyn : t -> Dyn.t
  val fold_right : t -> init:'acc -> f:(E.t -> 'acc -> 'acc) -> 'acc
  val tseq : Sem.t -> t -> Expr.t -> t -> t
  val initial : Expr.t -> t
  val empty : t
  val set_idx : Idx.t -> t -> t
  val hash : t -> int -> int
  val equal : t -> t -> bool
  val status : t -> Status.t
  val first_match : t -> Marks.t option
  val remove_matches : t -> t
  val split_at_match : t -> t * t
  val add_match : t -> Marks.t -> t
  val add_eps : t -> Marks.t -> t
  val add_expr : t -> E.t -> t
  val iter_marks : t -> f:(Marks.t -> unit) -> unit
  val remove_duplicates : Id.Hash_set.t -> t -> Expr.t -> t
end = struct
  module E = struct
    type t =
      | TSeq of Sem.t * t list * Expr.t
      | TExp of Marks.t * Expr.t
      | TMatch of Marks.t

    let rec equal_list l1 l2 = List.equal ~eq:equal l1 l2

    and equal x y =
      match x, y with
      | TSeq (_, l1, e1), TSeq (_, l2, e2) -> Id.equal e1.id e2.id && equal_list l1 l2
      | TExp (marks1, e1), TExp (marks2, e2) ->
        Id.equal e1.id e2.id && Marks.equal marks1 marks2
      | TMatch marks1, TMatch marks2 -> Marks.equal marks1 marks2
      | _, _ -> false
    ;;

    let rec hash (t : t) accu =
      match t with
      | TSeq (_, l, e) ->
        hash_combine 0x172a1bce (hash_combine (Id.hash e.id) (hash_list l accu))
      | TExp (marks, e) ->
        hash_combine 0x2b4c0d77 (hash_combine (Id.hash e.id) (Marks.hash marks accu))
      | TMatch marks -> hash_combine 0x1c205ad5 (Marks.hash marks accu)

    and hash_list =
      let f acc x = hash x acc in
      fun l init -> List.fold_left l ~init ~f
    ;;
  end

  type t = E.t list

  let rec to_dyn sem t = Dyn.list (List.map ~f:(dyn_of_e sem) t)

  and dyn_of_e sem =
    let open Dyn in
    function
    | E.TSeq (sem', x, y) ->
      wrap_sem
        sem
        sem'
        (variant "TSeq" [ to_dyn (Some sem') x; Expr.to_dyn (Some sem') y ])
    | TExp (marks, e) ->
      let e =
        let base = [ Expr.to_dyn sem e ] in
        if Marks.(equal empty marks) then base else Marks.to_dyn marks :: base
      in
      variant "TExp" e
    | TMatch m -> variant "TMarks" [ Marks.to_dyn m ]
  ;;

  let to_dyn t = to_dyn None t

  open E

  let equal = E.equal_list
  let hash = E.hash_list

  let tseq' kind x y =
    match x with
    | [] -> []
    | [ TExp (marks, { def = Eps; _ }) ] -> [ TExp (marks, y) ]
    | _ -> [ TSeq (kind, x, y) ]
  ;;

  let tseq kind x y rem = tseq' kind x y @ rem

  let rec fold_right t ~init ~f =
    match t with
    | [] -> init
    | x :: xs -> f x (fold_right xs ~init ~f)
  ;;

  let rec iter_marks t ~f =
    List.iter t ~f:(fun (e : E.t) ->
      match e with
      | TSeq (_, l, _) -> iter_marks l ~f
      | TExp (marks, _) | TMatch marks -> f marks)
  ;;

  let rec print_state_rec ch e (y : Expr.t) =
    match e with
    | TMatch marks -> Format.fprintf ch "@[<2>(TMatch@ %a)@]" Marks.pp marks
    | TSeq (sem, l', x) ->
      Format.fprintf ch "@[<2>(TSeq@ %a@ " Sem.pp sem;
      print_state_lst ch l' x;
      Format.fprintf ch "@ %a)@]" Expr.pp x
    | TExp (marks, { def = Eps; _ }) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ (eps))@]" Id.pp y.id Marks.pp marks
    | TExp (marks, x) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ %a)@]" Id.pp x.id Marks.pp marks Expr.pp x

  and print_state_lst ch l y =
    match l with
    | [] -> Format.fprintf ch "()"
    | e :: rem ->
      print_state_rec ch e y;
      List.iter rem ~f:(fun e ->
        Format.fprintf ch "@ | ";
        print_state_rec ch e y)
  ;;

  let pp ch t = print_state_lst ch [ t ] { id = Id.zero; def = Eps }

  let rec first_match = function
    | [] -> None
    | TMatch marks :: _ -> Some marks
    | _ :: r -> first_match r
  ;;

  let remove_matches t =
    List.filter t ~f:(function
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

  let status : _ -> Status.t = function
    | [] -> Failed
    | TMatch m :: _ -> Match (Mark_infos.make (m.marks :> (int * int) list), m.pmarks)
    | _ -> Running
  ;;

  let set_idx =
    let rec f idx = function
      | TMatch marks -> TMatch (Marks.marks_set_idx marks idx)
      | TSeq (kind, l, x) -> TSeq (kind, set_idx idx l, x)
      | TExp (marks, x) -> TExp (Marks.marks_set_idx marks idx, x)
    and set_idx idx xs = List.map xs ~f:(f idx) in
    set_idx
  ;;

  let[@ocaml.warning "-32"] pp fmt t =
    Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(Fmt.lit "; ") pp) t
  ;;

  let empty = []
  let initial expr = [ TExp (Marks.empty, expr) ]
  let add_match t marks = TMatch marks :: t
  let add_eps t marks = TExp (marks, eps_expr) :: t
  let add_expr t expr = expr :: t

  let remove_duplicates =
    let rec loop seen l y =
      match l with
      | [] -> []
      | (TMatch _ as x) :: _ ->
        (* Truncate after first match *)
        [ x ]
      | TSeq (kind, l, x) :: r ->
        let l = loop seen l x in
        let r = loop seen r y in
        tseq kind l x r
      | (TExp (_marks, { def = Eps; _ }) as e) :: r ->
        if Id.Hash_set.mem seen y.id
        then loop seen r y
        else (
          Id.Hash_set.add seen y.id;
          e :: loop seen r y)
      | (TExp (_marks, x) as e) :: r ->
        if Id.Hash_set.mem seen x.id
        then loop seen r y
        else (
          Id.Hash_set.add seen x.id;
          e :: loop seen r y)
    in
    fun seen l y ->
      Id.Hash_set.clear seen;
      loop seen l y
  ;;
end

module E = Desc.E

module State = struct
  type t =
    { idx : Idx.t
    ; category : Category.t
    ; desc : Desc.t
    ; mutable status : Status.t option
    ; hash : int
    }
  (* Thread-safety: We use double-checked locking to access field
     [status] in function [status] below. *)

  let pp fmt t = Desc.pp fmt t.desc
  let[@inline] idx t = t.idx
  let to_dyn t = Desc.to_dyn t.desc

  let dummy =
    { idx = Idx.unknown
    ; category = Category.dummy
    ; desc = Desc.empty
    ; status = None
    ; hash = -1
    }
  ;;

  let hash idx cat desc =
    Desc.hash desc (hash_combine idx (hash_combine (Category.to_int cat) 0))
    land 0x3FFFFFFF
  ;;

  let mk idx cat desc =
    { idx; category = cat; desc; status = None; hash = hash (idx :> int) cat desc }
  ;;

  let create cat e = mk Idx.initial cat (Desc.initial e)

  let equal { idx; category; desc; status = _; hash } t =
    Int.equal hash t.hash
    && Idx.equal idx t.idx
    && Category.equal category t.category
    && Desc.equal desc t.desc
  ;;

  (* To be called when the mutex has already been acquired *)
  let status_no_mutex s =
    match s.status with
    | Some s -> s
    | None ->
      let st = Desc.status s.desc in
      s.status <- Some st;
      st
  ;;

  let status m s =
    match s.status with
    | Some s -> s
    | None ->
      Mutex.lock m;
      let st = status_no_mutex s in
      Mutex.unlock m;
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
    ; seen : Id.Hash_set.t
    ; index_count : int Atomic.t
    }

  let create () =
    { ids = Bit_vector.create_zero 1
    ; seen = Id.Hash_set.create ()
    ; index_count = Atomic.make 0
    }
  ;;

  let index_count w = Atomic.get w.index_count

  let mark_used_indices tbl =
    Desc.iter_marks ~f:(fun marks ->
      List.iter marks.marks ~f:(fun (_, i) ->
        if Idx.used i then Bit_vector.set tbl (i :> int) true))
  ;;

  let rec find_free tbl idx len =
    if idx = len || not (Bit_vector.get tbl idx) then idx else find_free tbl (idx + 1) len
  ;;

  let free_index t l =
    Bit_vector.reset_zero t.ids;
    mark_used_indices t.ids l;
    let len = Bit_vector.length t.ids in
    let idx = find_free t.ids 0 len in
    if idx = len
    then (
      t.ids <- Bit_vector.create_zero (2 * len);
      (* This function is only called when the mutex is locked. So we
         are sure that this is always coherent with the length of
         [t.ids]. *)
      Atomic.set t.index_count (2 * len));
    Idx.make idx
  ;;
end

(**** Computation of the next state ****)

type ctx =
  { c : Cset.c
  ; prev_cat : Category.t
  ; next_cat : Category.t
  }

let rec delta_expr ({ c; _ } as ctx) marks (x : Expr.t) rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
  | Cst s -> if Cset.mem c s then Desc.add_eps rem marks else rem
  | Alt l -> delta_alt ctx marks l rem
  | Seq (kind, y, z) ->
    let y = delta_expr ctx marks y Desc.empty in
    delta_seq ctx kind y z rem
  | Rep (rep_kind, kind, y) -> delta_rep ctx marks x rep_kind kind y rem
  | Eps -> Desc.add_match rem marks
  | Mark i -> Desc.add_match rem (Marks.set_mark marks i)
  | Pmark i -> Desc.add_match rem (Marks.set_pmark marks i)
  | Erase (b, e) -> Desc.add_match rem (Marks.filter marks b e)
  | Before cat ->
    if Category.intersect ctx.next_cat cat then Desc.add_match rem marks else rem
  | After cat ->
    if Category.intersect ctx.prev_cat cat then Desc.add_match rem marks else rem

and delta_rep ctx marks x rep_kind kind y rem =
  let y, marks' =
    let y = delta_expr ctx marks y Desc.empty in
    match Desc.first_match y with
    | None -> y, marks
    | Some marks -> Desc.remove_matches y, marks
  in
  match rep_kind with
  | `Greedy -> Desc.tseq kind y x (Desc.add_match rem marks')
  | `Non_greedy -> Desc.add_match (Desc.tseq kind y x rem) marks

and delta_alt ctx marks l rem = List.fold_right l ~init:rem ~f:(delta_expr ctx marks)

and delta_seq ctx (kind : Sem.t) y z rem =
  match Desc.first_match y with
  | None -> Desc.tseq kind y z rem
  | Some marks ->
    (match kind with
     | `Longest -> Desc.tseq kind (Desc.remove_matches y) z (delta_expr ctx marks z rem)
     | `Shortest -> delta_expr ctx marks z (Desc.tseq kind (Desc.remove_matches y) z rem)
     | `First ->
       let y, y' = Desc.split_at_match y in
       Desc.tseq kind y z (delta_expr ctx marks z (Desc.tseq kind y' z rem)))
;;

let rec delta_e ctx marks (x : E.t) rem =
  match x with
  | TSeq (kind, y, z) ->
    let y = delta_desc ctx marks y Desc.empty in
    delta_seq ctx kind y z rem
  | TExp (marks, e) -> delta_expr ctx marks e rem
  | TMatch _ -> Desc.add_expr rem x

and delta_desc ctx marks (l : Desc.t) rem =
  Desc.fold_right l ~init:rem ~f:(fun y acc -> delta_e ctx marks y acc)
;;

let delta (tbl_ref : Working_area.t) next_cat char (st : State.t) =
  let expr =
    let prev_cat = st.category in
    let ctx = { c = char; next_cat; prev_cat } in
    Desc.remove_duplicates
      tbl_ref.seen
      (delta_desc ctx Marks.empty st.desc Desc.empty)
      Expr.eps_expr
  in
  let idx = Working_area.free_index tbl_ref expr in
  let expr = Desc.set_idx idx expr in
  State.mk idx next_cat expr
;;
