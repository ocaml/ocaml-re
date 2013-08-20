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

(* Regular expressions *)

type category = int
type mark = int

type sem = [ `Longest | `Shortest | `First ]
type rep_kind = [ `Greedy | `Non_greedy ]

type expr
type def =
    Cst of Cset.t
  | Alt of expr list
  | Seq of sem * expr * expr
  | Eps
  | Rep of rep_kind * sem * expr
  | Mark of mark
  | Erase of mark * mark
  | Before of category
  | After of category
val def : expr -> def
val print_expr : Format.formatter -> expr -> unit

type ids
val create_ids : unit -> ids

val cst : ids -> Cset.t -> expr
val empty : ids -> expr
val alt : ids -> expr list -> expr
val seq : ids -> sem -> expr -> expr -> expr
val eps : ids -> expr
val rep : ids -> rep_kind -> sem -> expr -> expr
val mark : ids -> mark -> expr
val erase : ids -> mark -> mark -> expr
val before : ids -> category -> expr
val after : ids -> category -> expr

val rename : ids -> expr -> expr

(****)

(* States of the automata *)

type idx = int
type mark_offsets = (mark * idx) list
type e =
    TSeq of e list * expr * sem
  | TExp of mark_offsets * expr
  | TMatch of mark_offsets

val print_state : Format.formatter -> e list -> unit

type hash
type mark_infos = int array
type status = [`Failed | `Match of mark_infos | `Running]
type state =
  idx * category * e list * status option ref * hash
val dummy_state : state
val mk_state : idx -> category -> e list -> state
val create_state : category -> expr -> state
module States : Hashtbl.S with type key = state

(****)

(* Computation of the states following a given state *)

type working_area
val create_working_area : unit -> working_area
val index_count : working_area -> int

val delta : working_area -> category -> Cset.c -> state -> state
val deriv :
  working_area -> Cset.t -> (category * Cset.t) list -> state ->
  (Cset.t * state) list

(****)

val status : state -> status
