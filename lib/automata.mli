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

type mark = int

type sem =
  [ `Longest
  | `Shortest
  | `First
  ]

type rep_kind =
  [ `Greedy
  | `Non_greedy
  ]

val pp_sem : Format.formatter -> sem -> unit
val pp_rep_kind : Format.formatter -> rep_kind -> unit

type expr

val is_eps : expr -> bool
val pp : Format.formatter -> expr -> unit

module Ids : sig
  type t

  val create : unit -> t
end

val cst : Ids.t -> Cset.t -> expr
val empty : Ids.t -> expr
val alt : Ids.t -> expr list -> expr
val seq : Ids.t -> sem -> expr -> expr -> expr
val eps : Ids.t -> expr
val rep : Ids.t -> rep_kind -> sem -> expr -> expr
val mark : Ids.t -> mark -> expr
val pmark : Ids.t -> Pmark.t -> expr
val erase : Ids.t -> mark -> mark -> expr
val before : Ids.t -> Category.t -> expr
val after : Ids.t -> Category.t -> expr
val rename : Ids.t -> expr -> expr

(****)

(* States of the automata *)

type idx = int

type status =
  | Failed
  | Match of Mark_infos.t * Pmark.Set.t
  | Running

module State : sig
  type t

  val dummy : t
  val create : Category.t -> expr -> t
  val idx : t -> idx
  val status : t -> status

  module Table : Hashtbl.S with type key = t
end

(****)

(* Computation of the states following a given state *)

module Working_area : sig
  type t

  val create : unit -> t
  val index_count : t -> int
end

val delta : Working_area.t -> Category.t -> Cset.c -> State.t -> State.t

val deriv
  :  Working_area.t
  -> Cset.t
  -> (Category.t * Cset.t) list
  -> State.t
  -> (Cset.t * State.t) list

(****)
