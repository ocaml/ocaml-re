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

open Import

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

    module Hash_set = struct
      type t =
        { mutable stamps : bytes
        ; mutable generation : char
        }

      let create () = { stamps = Bytes.empty; generation = '\001' }

      let mem t id =
        id < Bytes.length t.stamps
        && Char.equal (Bytes.unsafe_get t.stamps id) t.generation
      ;;

      let add t id =
        if id >= Bytes.length t.stamps
        then (
          let stamps = Bytes.make (max (id + 1) (2 * Bytes.length t.stamps)) '\000' in
          Bytes.blit t.stamps 0 stamps 0 (Bytes.length t.stamps);
          t.stamps <- stamps);
        Bytes.unsafe_set t.stamps id t.generation
      ;;

      let clear t =
        if Char.equal t.generation '\255'
        then (
          Bytes.fill t.stamps 0 (Bytes.length t.stamps) '\000';
          t.generation <- '\001')
        else t.generation <- Char.unsafe_chr (Char.code t.generation + 1)
      ;;
    end

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

  let to_string_short = function
    | `Shortest -> "S"
    | `Longest -> "L"
    | `First -> "F"
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

  let to_string_short = function
    | `Greedy -> "G"
    | `Non_greedy -> "N"
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
      (* In Seq (sem, t1, t2), sem describes which match of t1 is
         preferred, but says nothing about t2 *)
    | Eps
    | Rep of Rep_kind.t * Sem.t * t
    | Mark of Mark.t
    | Erase of Mark.t * Mark.t
    | Before of Category.t
    | After of Category.t
    | Pmark of Pmark.t

  let rec seq_as_list sem t =
    match t.def with
    | Eps -> []
    | Seq (sem', x, y) when Sem.equal sem sem' -> x :: seq_as_list sem y
    | _ -> [ t ]
  ;;

  let sem_kind_suffix ?kind sem =
    ":"
    ^ (match kind with
       | None -> ""
       | Some k -> Rep_kind.to_string_short k)
    ^ Sem.to_string_short sem
  ;;

  let rec dyn_of_def =
    let open Dyn in
    function
    | Cst cset -> Cset.to_dyn cset
    | Alt alt -> variant "Alt" (List.map ~f:to_dyn alt)
    | Seq (sem, x, y) ->
      let y = seq_as_list sem y in
      variant ("Seq" ^ sem_kind_suffix sem) (to_dyn x :: List.map y ~f:to_dyn)
    | Eps -> Enum "Eps"
    | Rep (kind, sem, t) -> variant ("Rep" ^ sem_kind_suffix ~kind sem) [ to_dyn t ]
    | Mark m -> variant "Mark" [ Mark.to_dyn m ]
    | Pmark m -> variant "Pmark" [ Pmark.to_dyn m ]
    | Erase (x, y) -> variant "Erase" [ Mark.to_dyn x; Mark.to_dyn y ]
    | Before c -> variant "Before" [ Category.to_dyn c ]
    | After c -> variant "After" [ Category.to_dyn c ]

  and to_dyn { id = _; def } = dyn_of_def def

  let rec pp ch e =
    let open Fmt in
    match e.def with
    | Cst l -> sexp ch "cst" Cset.pp l
    | Alt l -> sexp ch "alt" (list pp) l
    | Seq (k, e, e') -> sexp ch "seq" (triple Sem.pp pp pp) (k, e, e')
    | Eps -> str ch "eps"
    | Rep (rk, k, e) -> sexp ch "rep" (triple Rep_kind.pp Sem.pp pp) (rk, k, e)
    | Mark i -> sexp ch "mark" Mark.pp i
    | Pmark i -> sexp ch "pmark" Pmark.pp i
    | Erase (b, e) -> sexp ch "erase" (pair Mark.pp Mark.pp) (b, e)
    | Before c -> sexp ch "before" Category.pp c
    | After c -> sexp ch "after" Category.pp c
  ;;

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

  let rec equal_marks xs ys =
    Phys_equal.equal xs ys
    ||
    match xs, ys with
    | (x, i) :: xs, (y, j) :: ys -> Mark.equal x y && Idx.equal i j && equal_marks xs ys
    | _, _ -> false
  ;;

  let equal m ({ marks; pmarks } as t) =
    Phys_equal.equal m t || (equal_marks m.marks marks && Pmark.Set.equal m.pmarks pmarks)
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
      | (a, i) :: rem when Idx.equal i Idx.unknown -> (a, idx) :: marks_set_idx idx rem
      | _ -> marks
    in
    fun t idx ->
      match t.marks with
      | (_, i) :: _ when Idx.equal i Idx.unknown ->
        { t with marks = marks_set_idx idx t.marks }
      | _ -> t
  ;;

  let filter t (b : Mark.t) (e : Mark.t) =
    { t with
      marks =
        List.filter t.marks ~f:(fun ((i : Mark.t), _) ->
          Mark.outside_range i ~start_inclusive:b ~stop_inclusive:e)
    }
  ;;

  let rec remove_mark i marks =
    match marks with
    | [] -> []
    | (j, _) :: rest when Mark.equal i j -> rest
    | mark :: rest ->
      let rest' = remove_mark i rest in
      if Phys_equal.equal rest rest' then marks else mark :: rest'
  ;;

  let set_mark t (i : Mark.t) =
    { t with marks = (i, Idx.unknown) :: remove_mark i t.marks }
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
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
            (fun fmt (a, i) -> Format.fprintf fmt "%a-%a" Mark.pp a Idx.pp i))
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
  type no_match
  type has_match

  (* Each final [node] links to the remaining alternatives. *)
  type _ node = private
    | Empty : no_match node
    | TSeq : Sem.t * no_match node * Expr.t * 'k node -> 'k node
    | TExp : Marks.t * Expr.t * 'k node -> 'k node
    | TMatch : Marks.t * 'k node -> has_match node

  type t = Desc : 'k node -> t [@@unboxed]

  type _ match_result =
    | No_match : no_match match_result
    | First_match : Marks.t -> has_match match_result

  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val tseq : Sem.t -> no_match node -> Expr.t -> t -> t
  val texp : Marks.t -> Expr.t -> t -> t
  val initial : Expr.t -> t
  val empty : t
  val set_idx : Idx.t -> t -> t
  val hash : t -> int -> int
  val equal : t -> t -> bool
  val status : t -> Status.t
  val status_is_ambiguous : t -> ambiguity_mark:Pmark.t -> bool
  val first_match : 'k node -> 'k match_result
  val remove_matches : has_match node -> no_match node
  val split_at_match : has_match node -> no_match node * no_match node
  val add_match : t -> Marks.t -> t
  val add_eps : t -> Marks.t -> t
  val iter_marks : t -> f:(Marks.t -> unit) -> unit
  val remove_duplicates : Id.Hash_set.t -> t -> Expr.t -> t
end = struct
  (* TMatch is produced when deriving a regex succeeds without consuming the
     input character. Derivation therefore proceeds into whatever follows the
     TMatch. Matches are dropped by remove_matches or split_at_match, or bubble
     up to the top, so they only occur in the outermost chain, never inside a TSeq.

     The index records whether the alternative chain contains a TMatch. A TSeq
     can only nest a chain without one; sequencing must first bubble matches out. *)
  type no_match = [ `No_match ]
  type has_match = [ `Has_match ]

  type _ node =
    | Empty : no_match node
    | TSeq : Sem.t * no_match node * Expr.t * 'k node -> 'k node
    | TExp : Marks.t * Expr.t * 'k node -> 'k node
    | TMatch : Marks.t * 'k node -> has_match node

  type t = Desc : 'k node -> t [@@unboxed]

  type _ match_result =
    | No_match : no_match match_result
    | First_match : Marks.t -> has_match match_result

  let rec equal (Desc x as a) (Desc y as b) =
    Phys_equal.equal a b
    ||
    match x, y with
    | TSeq (_, l1, e1, rest1), TSeq (_, l2, e2, rest2) ->
      Id.equal e1.id e2.id && equal (Desc l1) (Desc l2) && equal (Desc rest1) (Desc rest2)
    | TExp (m1, e1, rest1), TExp (m2, e2, rest2) ->
      Id.equal e1.id e2.id && Marks.equal m1 m2 && equal (Desc rest1) (Desc rest2)
    | TMatch (m1, rest1), TMatch (m2, rest2) ->
      Marks.equal m1 m2 && equal (Desc rest1) (Desc rest2)
    | _, _ -> false
  ;;

  let rec hash (Desc t) accu =
    match t with
    | Empty -> accu
    | TSeq (_, l, e, rest) ->
      hash
        (Desc rest)
        (hash_combine 0x172a1bce (hash_combine (Id.hash e.id) (hash (Desc l) accu)))
    | TExp (m, e, rest) ->
      hash
        (Desc rest)
        (hash_combine 0x2b4c0d77 (hash_combine (Id.hash e.id) (Marks.hash m accu)))
    | TMatch (m, rest) -> hash (Desc rest) (hash_combine 0x1c205ad5 (Marks.hash m accu))
  ;;

  let rec to_dyn t = Dyn.list (dyn_of_nodes t)

  and dyn_of_nodes (Desc t) =
    let open Dyn in
    match t with
    | Empty -> []
    | TSeq (sem, x, y, rest) ->
      variant ("TSeq" ^ sem_kind_suffix sem) [ to_dyn (Desc x); Expr.to_dyn y ]
      :: dyn_of_nodes (Desc rest)
    | TExp (marks, e, rest) ->
      let base = [ Expr.to_dyn e ] in
      variant
        "TExp"
        (if Marks.(equal empty marks) then base else Marks.to_dyn marks :: base)
      :: dyn_of_nodes (Desc rest)
    | TMatch (m, rest) -> variant "TMatch" [ Marks.to_dyn m ] :: dyn_of_nodes (Desc rest)
  ;;

  let tseq_node kind (x : no_match node) y rem =
    match x with
    | Empty -> rem
    | TExp (marks, { def = Eps; _ }, Empty) -> TExp (marks, y, rem)
    | _ -> TSeq (kind, x, y, rem)
  ;;

  let tseq kind x y (Desc rem) = Desc (tseq_node kind x y rem)
  let texp marks e (Desc rem) = Desc (TExp (marks, e, rem))

  let rec iter_marks (Desc t) ~f =
    match t with
    | Empty -> ()
    | TSeq (_, l, _, rest) ->
      iter_marks (Desc l) ~f;
      iter_marks (Desc rest) ~f
    | TExp (marks, _, rest) ->
      f marks;
      iter_marks (Desc rest) ~f
    | TMatch (marks, rest) ->
      f marks;
      iter_marks (Desc rest) ~f
  ;;

  let rec print_state_lst ch ~(sep : (_, _, _) format) (Desc e) (y : Expr.t) =
    match e with
    | Empty -> Format.fprintf ch "()"
    | TMatch (marks, rest) ->
      Format.fprintf ch "@[<2>(TMatch@ %a)@]" Marks.pp marks;
      print_state_rest ch ~sep (Desc rest) y
    | TSeq (sem, l', x, rest) ->
      Format.fprintf ch "@[<2>(TSeq@ %a@ " Sem.pp sem;
      print_state_lst ch ~sep:"@ | " (Desc l') x;
      Format.fprintf ch "@ %a)@]" Expr.pp x;
      print_state_rest ch ~sep (Desc rest) y
    | TExp (marks, { def = Eps; _ }, rest) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ (eps))@]" Id.pp y.id Marks.pp marks;
      print_state_rest ch ~sep (Desc rest) y
    | TExp (marks, x, rest) ->
      Format.fprintf ch "@[<2>(TExp@ %a@ (%a)@ %a)@]" Id.pp x.id Marks.pp marks Expr.pp x;
      print_state_rest ch ~sep (Desc rest) y

  and print_state_rest ch ~sep (Desc rest as t) y =
    match rest with
    | Empty -> ()
    | _ ->
      Format.fprintf ch sep;
      print_state_lst ch ~sep t y
  ;;

  let pp ch (Desc e as t) =
    Format.fprintf ch "[";
    (match e with
     | Empty -> ()
     | _ -> print_state_lst ch ~sep:";@ " t { id = Id.zero; def = Eps });
    Format.fprintf ch "]"
  ;;

  let rec first_match : type k. k node -> k match_result = function
    | Empty -> No_match
    | TMatch (marks, _) -> First_match marks
    | TExp (_, _, rest) -> first_match rest
    | TSeq (_, _, _, rest) -> first_match rest
  ;;

  type _ presence =
    | Absent : no_match presence
    | Present : has_match presence

  (* Unlike [first_match], this check does not allocate a result carrying marks.
     Its index lets us return an entirely match-free suffix without rebuilding it. *)
  let rec presence : type k. k node -> k presence = function
    | Empty -> Absent
    | TMatch _ -> Present
    | TExp (_, _, rest) -> presence rest
    | TSeq (_, _, _, rest) -> presence rest
  ;;

  (* Every node before a known match must change, so no physical-equality checks
     are needed on that prefix. After the match, [presence] lets us reuse a clean
     suffix. A suffix containing another match is scanned and then rebuilt. *)
  let rec remove_matches : has_match node -> no_match node = function
    | TMatch (_, rest) -> remove_any_matches rest
    | TExp (m, e, rest) -> TExp (m, e, remove_matches rest)
    | TSeq (k, l, e, rest) -> TSeq (k, l, e, remove_matches rest)

  and remove_any_matches : type k. k node -> no_match node =
    fun t ->
    match presence t with
    | Absent -> t
    | Present -> remove_matches t
  ;;

  let rec split_at_match : has_match node -> no_match node * no_match node = function
    | TMatch (_, rest) -> Empty, remove_any_matches rest
    | TExp (m, e, rest) ->
      let before, after = split_at_match rest in
      TExp (m, e, before), after
    | TSeq (k, inner, e, rest) ->
      let before, after = split_at_match rest in
      TSeq (k, inner, e, before), after
  ;;

  let status (Desc t) : Status.t =
    match t with
    | Empty -> Failed
    | TMatch (m, _) -> Match (Mark_infos.make (m.marks :> (int * int) list), m.pmarks)
    | _ -> Running
  ;;

  let status_is_ambiguous (Desc t) ~ambiguity_mark =
    match t with
    | TMatch (m, _) -> Pmark.Set.mem ambiguity_mark m.pmarks
    | _ -> false
  ;;

  let rec set_idx_node : type k. Idx.t -> k node -> k node =
    fun idx -> function
    | Empty -> Empty
    | TMatch (m, rest) as t ->
      let m' = Marks.marks_set_idx m idx in
      let rest' = set_idx_node idx rest in
      if Phys_equal.equal m m' && Phys_equal.equal rest rest'
      then t
      else TMatch (m', rest')
    | TExp (m, e, rest) as t ->
      let m' = Marks.marks_set_idx m idx in
      let rest' = set_idx_node idx rest in
      if Phys_equal.equal m m' && Phys_equal.equal rest rest'
      then t
      else TExp (m', e, rest')
    | TSeq (k, l, e, rest) as t ->
      let l' = set_idx_node idx l in
      let rest' = set_idx_node idx rest in
      if Phys_equal.equal l l' && Phys_equal.equal rest rest'
      then t
      else TSeq (k, l', e, rest')
  ;;

  let set_idx idx (Desc t) = Desc (set_idx_node idx t)
  let empty = Desc Empty
  let initial expr = Desc (TExp (Marks.empty, expr, Empty))
  let add_match (Desc t) marks = Desc (TMatch (marks, t))
  let add_eps (Desc t) marks = Desc (TExp (marks, eps_expr, t))

  let remove_duplicates =
    (* Removing duplicates is necessary for the automata to be finite.
       Given an input:

       [ TSeq ([TExp exp1], exp2); TSeq ([TExp exp1], exp3) ]

       remove_duplicates removes the second exp1 as a duplicate, thus turning the
       list into:

       [ TSeq ([TExp exp1], exp2) ]

       If exp2 and exp3 were arbitrary expression, this would obviously be incorrect.
       This is justified by the invariant that TExp exp1 is necessarily followed by its
       continuation from the original expr. Because exp1 is unique across the expr
       (even though [repn r 5 None] duplicates [r], [Expr.rename] ensures their ids are
       made unique), it has a single continuation, and thus exp2 = exp3.

       The reasoning doesn't obviously hold for rep a. rep a is effectively the infinite
       sequence a(a(a(...)|eps)|eps)|eps, with the _same_ id for each a. So each
       occurence of a and rep a still have a single continuation, namely rep a and
       whatever comes syntactically after rep a.
    *)
    let rec loop : type k. Id.Hash_set.t -> k node -> Expr.t -> k node =
      fun seen l y ->
      match l with
      | Empty -> Empty
      | TMatch (m, rest) as t ->
        (* Truncate after first match *)
        (match rest with
         | Empty -> t
         | _ -> TMatch (m, Empty))
      | TSeq (kind, l, x, rest) as t ->
        let l' = loop seen l x in
        let rest' = loop seen rest y in
        if Phys_equal.equal l l' && Phys_equal.equal rest rest'
        then t
        else tseq_node kind l' x rest'
      | TExp (marks, ({ def = Eps; _ } as x), rest) as t ->
        if Id.Hash_set.mem seen y.id
        then loop seen rest y
        else (
          Id.Hash_set.add seen y.id;
          let rest' = loop seen rest y in
          if Phys_equal.equal rest rest' then t else TExp (marks, x, rest'))
      | TExp (marks, x, rest) as t ->
        if Id.Hash_set.mem seen x.id
        then loop seen rest y
        else (
          Id.Hash_set.add seen x.id;
          let rest' = loop seen rest y in
          if Phys_equal.equal rest rest' then t else TExp (marks, x, rest'))
    in
    fun seen (Desc l) y ->
      Id.Hash_set.clear seen;
      Desc (loop seen l y)
  ;;
end

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

  let mk idx ~masked_cat desc =
    { idx
    ; category = masked_cat
    ; desc
    ; status = None
    ; hash = hash (idx :> int) masked_cat desc
    }
  ;;

  let create cat e = mk Idx.initial ~masked_cat:cat (Desc.initial e)

  let equal s t =
    Phys_equal.equal s t
    || (Int.equal s.hash t.hash
        && Idx.equal s.idx t.idx
        && Category.equal s.category t.category
        && Desc.equal s.desc t.desc)
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
    ; after_category_mask : Category.t
    }

  let create after_category_mask =
    { ids = Bit_vector.create_zero 1
    ; seen = Id.Hash_set.create ()
    ; index_count = Atomic.make 0
    ; after_category_mask
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
  | Delta of
      { c : Cset.c
      ; prev_cat : Category.t
      ; next_cat : Category.t
      }
  | Advance of
      { prev_cat : Category.t
      ; ambiguity_mark : Pmark.t
      }

let rec delta_expr ctx marks (x : Expr.t) rem =
  (*Format.eprintf "%d@." x.id;*)
  match x.def with
  | Cst s ->
    (match ctx with
     | Delta { c; _ } -> if Cset.mem c s then Desc.add_eps rem marks else rem
     | Advance _ -> Desc.texp marks x rem)
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
    (match ctx with
     | Delta { next_cat; _ } ->
       if Category.intersect next_cat cat then Desc.add_match rem marks else rem
     | Advance { ambiguity_mark; _ } ->
       Desc.add_match rem (Marks.set_pmark marks ambiguity_mark))
  | After cat ->
    let prev_cat =
      match ctx with
      | Delta { prev_cat; _ } -> prev_cat
      | Advance { prev_cat; _ } -> prev_cat
    in
    if Category.intersect prev_cat cat then Desc.add_match rem marks else rem

and delta_rep ctx marks x rep_kind kind y rem =
  let ((y, marks') : Desc.no_match Desc.node * Marks.t) =
    let (Desc.Desc y) = delta_expr ctx marks y Desc.empty in
    match Desc.first_match y with
    | No_match -> y, marks
    | First_match marks -> Desc.remove_matches y, marks
  in
  match rep_kind with
  | `Greedy -> Desc.tseq kind y x (Desc.add_match rem marks')
  | `Non_greedy -> Desc.add_match (Desc.tseq kind y x rem) marks

and delta_alt ctx marks l rem = List.fold_right l ~init:rem ~f:(delta_expr ctx marks)

and delta_seq ctx (kind : Sem.t) (Desc.Desc y) z rem =
  match Desc.first_match y with
  | No_match -> Desc.tseq kind y z rem
  | First_match marks ->
    (match kind with
     | `Longest -> Desc.tseq kind (Desc.remove_matches y) z (delta_expr ctx marks z rem)
     | `Shortest -> delta_expr ctx marks z (Desc.tseq kind (Desc.remove_matches y) z rem)
     | `First ->
       let y, y' = Desc.split_at_match y in
       Desc.tseq kind y z (delta_expr ctx marks z (Desc.tseq kind y' z rem)))
;;

let rec delta_desc ctx (Desc.Desc x as original) rem =
  match x with
  | Empty -> rem
  | TSeq (kind, y, z, rest) ->
    let rem = delta_desc ctx (Desc.Desc rest) rem in
    let y = delta_desc ctx (Desc.Desc y) Desc.empty in
    delta_seq ctx kind y z rem
  | TExp (marks, e, rest) -> delta_expr ctx marks e (delta_desc ctx (Desc.Desc rest) rem)
  | TMatch (marks, rest) ->
    let rem = delta_desc ctx (Desc.Desc rest) rem in
    if Phys_equal.equal (Desc.Desc rest) rem then original else Desc.add_match rem marks
;;

let create_state tbl_ref next_cat expr =
  let idx = Working_area.free_index tbl_ref expr in
  let expr = Desc.set_idx idx expr in
  State.mk idx ~masked_cat:(Category.mask next_cat tbl_ref.after_category_mask) expr
;;

let delta (tbl_ref : Working_area.t) next_cat char (st : State.t) =
  let expr =
    let prev_cat = st.category in
    let ctx = Delta { c = char; next_cat; prev_cat } in
    Desc.remove_duplicates tbl_ref.seen (delta_desc ctx st.desc Desc.empty) Expr.eps_expr
  in
  create_state tbl_ref next_cat expr
;;

(* When deriving [Re.str "abc"] wrt "a" then "b" then "c", we end up with [T (marks,
   Eps)], i.e. not a match, until the next character. When matching whole strings, this
   is fine because we feed in an eos character (in final_advance) if necessary. For
   exec_partial though, it means we'd return `Prefix too conservatively. So here we
   advance through all epsilon transitions, so that we can detect a match or a mismatch
   without waiting for an extra character.

   The wrinkle in this story is lookaheads, i.e. Before. We can't treat them as matches,
   otherwise [exec_partial eol ""] would incorrectly say `Full. We can't leave them
   unchanged, otherwise [exec_partial (shortest (alt [ eos; group bos ])) ""] would
   return [`Full (second branch)], when the eos branch can also match.

   Without implementing full-blown lookaheads, a solution that's enough for exec_partial
   can be found by noticing that:
   - if the looahead matches, the re that follows it ends up at a certain place in the
     resulting desc
   - if the lookahead doesn't match, that same place would hold a lower priority re that
     was shadowed by the lookahead match, or nothing if there is no lower priority re.

   This means that we can assume the lookahead matches, and if the resulting
   Desc.status doesn't depend on that assumption (which we track using this
   ambiguity_mark), then it means this status is independent of the lookahead. If we
   find the ambiguity mark, then obviously the result depends on the lookahead, so we
   can't use it.

   Do not compute more transitions from the resulting state! Even when the status
   is not ambiguous, the desc can still contain assumptions. *)
let advance (tbl_ref : Working_area.t) (st : State.t) =
  let expr =
    let ambiguity_mark = Pmark.gen () in
    let prev_cat = st.category in
    let ctx = Advance { prev_cat; ambiguity_mark } in
    let desc =
      Desc.remove_duplicates
        tbl_ref.seen
        (delta_desc ctx st.desc Desc.empty)
        Expr.eps_expr
    in
    if Desc.status_is_ambiguous desc ~ambiguity_mark then st.desc else desc
  in
  create_state tbl_ref st.category expr
;;
