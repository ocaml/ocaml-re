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

(* Regular expressions *)

type category = int
type mark = int

type sem = [ `Longest | `Shortest | `First ]
type rep_kind = [ `Greedy | `Non_greedy ]

val pp_sem : Format.formatter -> sem -> unit
val pp_rep_kind : Format.formatter -> rep_kind -> unit

module Pmark : sig
  type t = private int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val gen : unit -> t
  val pp : Format.formatter -> t -> unit
end

type expr
type def =
    Cst of Re_cset.t
  | Alt of expr list
  | Seq of sem * expr * expr
  | Eps
  | Rep of rep_kind * sem * expr
  | Mark of mark
  | Erase of mark * mark
  | Before of category
  | After of category
  | Pmark of Pmark.t
val def : expr -> def
val pp : Format.formatter -> expr -> unit

type ids
val create_ids : unit -> ids

val cst : ids -> Re_cset.t -> expr
val empty : ids -> expr
val alt : ids -> expr list -> expr
val seq : ids -> sem -> expr -> expr -> expr
val eps : ids -> expr
val rep : ids -> rep_kind -> sem -> expr -> expr
val mark : ids -> mark -> expr
val pmark : ids -> Pmark.t -> expr
val erase : ids -> mark -> mark -> expr
val before : ids -> category -> expr
val after : ids -> category -> expr

val rename : ids -> expr -> expr

(****)

module PmarkSet : Set.S with type elt = Pmark.t

(* States of the automata *)

type idx = int
type mark_offsets = {
  marks : (mark * idx) list ;
  pmarks : PmarkSet.t
}

module E : sig
  type t
end

val print_state : Format.formatter -> E.t list -> unit

type hash
type mark_infos = int array
type status = Failed | Match of mark_infos * PmarkSet.t | Running

module State : sig
  type t = idx * category * E.t list * status option ref * hash
  val dummy : t
  val create : category -> expr -> t
  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

type working_area
val create_working_area : unit -> working_area
val index_count : working_area -> int

val delta : working_area -> category -> Re_cset.c -> State.t -> State.t
val deriv :
  working_area -> Re_cset.t -> (category * Re_cset.t) list -> State.t ->
  (Re_cset.t * State.t) list

(****)

val status : State.t -> status
