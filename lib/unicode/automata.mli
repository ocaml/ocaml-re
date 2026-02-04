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

module Ids : sig
  type t

  val create : unit -> t
end

module Sem : sig
  type t = [ `Longest | `Shortest | `First ]

  val to_dyn : t -> Dyn.t
  val pp : t Fmt.t
end

module Rep_kind : sig
  type t = [ `Greedy | `Non_greedy ]

  val to_dyn : t -> Dyn.t
  val pp : t Fmt.t
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
end

module Idx : sig
  type t

  val to_int : t -> int
end

module Status : sig
  type t = Failed | Match of Mark_infos.t * Pmark.Set.t | Running
end

module type T = sig
  type cset
  type cp
  type category
  type expr

  val is_eps : expr -> bool
  val pp : expr Fmt.t
  val cst : Ids.t -> cset -> expr
  val empty : Ids.t -> expr
  val alt : Ids.t -> expr list -> expr
  val seq : Ids.t -> Sem.t -> expr -> expr -> expr
  val eps : Ids.t -> expr
  val rep : Ids.t -> Rep_kind.t -> Sem.t -> expr -> expr
  val mark : Ids.t -> Mark.t -> expr
  val pmark : Ids.t -> Pmark.t -> expr
  val erase : Ids.t -> Mark.t -> Mark.t -> expr
  val before : Ids.t -> category -> expr
  val after : Ids.t -> category -> expr
  val rename : Ids.t -> expr -> expr

  (****)

  (* States of the automata *)

  module State : sig
    type t

    val pp : t Fmt.t
    val dummy : t
    val create : category -> expr -> t
    val idx : t -> Idx.t
    val status_no_mutex : t -> Status.t
    val status : Mutex.t -> t -> Status.t
    val to_dyn : t -> Dyn.t

    module Table : Hashtbl.S with type key = t
  end

  (****)

  (* Computation of the states following a given state *)

  module Working_area : sig
    type t

    val create : unit -> t
    val index_count : t -> int
  end

  val delta : Working_area.t -> category -> cp -> State.t -> State.t
end

module Make (Cset : Cset.T) : T with type cset = Cset.t and type cp = Cset.cp and type category = Category.Make(Cset).t
