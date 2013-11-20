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

(* Character sets, represented as sorted list of intervals *)

type c = int
type t = (c * c) list

val union : t -> t -> t
val inter : t -> t -> t
val diff : t -> t -> t
val offset : int -> t -> t

val empty : t
val single : c -> t
val seq : c -> c -> t
val add : c -> t -> t

val mem : c -> t -> bool

type hash
val hash : t -> hash
module Map : Map.S with type key = hash * t

val print : Format.formatter -> t -> unit
