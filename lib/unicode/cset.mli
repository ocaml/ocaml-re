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

module type Categories = sig
  type c

  val cany : (c * c) list
  val ascii : (c * c) list
  val cdigit : (c * c) list
  val cupper : (c * c) list
  val clower : (c * c) list
  val cword : (c * c) list
  val calpha : (c * c) list
  val calnum : (c * c) list
  val xdigit : (c * c) list
  val lower : (c * c) list
  val upper : (c * c) list
  val alpha : (c * c) list
  val alnum : (c * c) list
  val wordc : (c * c) list
  val nl : (c * c) list
  val blank : (c * c) list
  val space : (c * c) list
  val cntrl : (c * c) list
  val graph : (c * c) list
  val print : (c * c) list
  val punct : (c * c) list
end

module type CodePage = sig
  type t [@@immediate]
  type letter

  val to_letter : t -> letter
  val from_letter : letter -> t

  (** special characters which isn't present in any set (not even in [cany]) *)
  val null : t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val succ : t -> t
  val pred : t -> t
  val max : t -> t -> t
  val min : t -> t -> t
  val max_t : t
  val min_t : t
  val offset : int -> t -> t
  val to_int : t -> int
  val of_int : int -> t
  val of_char : char -> letter
  val to_char : letter -> char
  val pp : Format.formatter -> t -> unit

  module Categories : Categories with type c := t
end

module type T = sig
  type cp [@@immediate]
  type letter
  type t

  module CodePage : CodePage with type t := cp and type letter := letter
  module Codec : Uucodecs.T with type letter = letter

  val equal : t -> t -> bool
  val iter : t -> f:(cp -> cp -> unit) -> unit
  val union : t -> t -> t
  val union_all : t list -> t
  val intersect_all : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val empty : t
  val single : cp -> t
  val add : cp -> t -> t
  val mem : cp -> t -> bool
  val case_insens : t -> t
  val cany : t
  val ascii : t
  val cdigit : t
  val cupper : t
  val clower : t
  val calpha : t
  val cword : t
  val notnl : t
  val nl : t
  val cseq : letter -> letter -> t
  val set : string -> t
  val blank : t
  val space : t
  val xdigit : t
  val lower : t
  val upper : t
  val alpha : t
  val alnum : t
  val wordc : t
  val cntrl : t
  val graph : t
  val print : t
  val punct : t
  val pp : Format.formatter -> t -> unit
  val one_c : t -> cp option
  val fold_left : t -> init:'acc -> f:('acc -> cp -> cp -> 'acc) -> 'acc
  val fold_right : t -> init:'acc -> f:(cp -> cp -> 'acc -> 'acc) -> 'acc
  val hash : t -> int
  val compare : t -> t -> int

  module CSetMap : Map.S with type key = int * t

  val csingle : letter -> t
  val is_empty : t -> bool
  val prepend : t -> 'a list -> (t * 'a list) list -> (t * 'a list) list
  val pick : t -> cp
  val offset : int -> t -> t
  val to_dyn : t -> Dyn.t
end

module Make : functor
  (Codec : Uucodecs.T)
  (_ : CodePage with type letter = Codec.letter)
  -> T with type letter = Codec.letter

module Utf8 : T with type letter = Uchar.t
module Utf16be : T with type letter = Uchar.t
module Utf16le : T with type letter = Uchar.t
module Latin1 : T with type letter = Char.t
