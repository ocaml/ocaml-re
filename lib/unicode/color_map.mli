(* Color maps exists to provide an optimization for the regex engine. The fact
   that some characters are entirely equivalent for some regexes means that we
   can use them interchangeably.

   A color map assigns a color to every character in our character set. Any two
   characters with the same color will be treated equivalently by the automaton.
*)

module type T = sig
  type cp
  type letter
  type cset_t
  type color = int
  type t

  module Repr : sig
    type t

    val repr : t -> color -> letter
    val length : t -> int
    val pp : Format.formatter -> t -> unit
  end

  module Table : sig
    type t

    val get : t -> letter -> cp
    val get_letter : t -> cp -> letter
    val translate_colors : t -> cset_t -> cset_t
    val pp : Format.formatter -> t -> unit
  end

  val make : unit -> t
  val flatten : t -> Table.t * Repr.t
  val split : t -> cset_t -> unit
  val pp : Format.formatter -> t -> unit
end

module Make (Cset : Cset.T) :
  T
    with type cp = Cset.cp
     and type letter = Cset.letter
     and type cset_t = Cset.t

module Utf8 :
  T
    with type cp = Cset.Utf8.cp
     and type letter = Cset.Utf8.letter
     and type cset_t = Cset.Utf8.t

module Utf16be :
  T
    with type cp = Cset.Utf16be.cp
     and type letter = Cset.Utf16be.letter
     and type cset_t = Cset.Utf16be.t

module Utf16le :
  T
    with type cp = Cset.Utf16le.cp
     and type letter = Cset.Utf16le.letter
     and type cset_t = Cset.Utf16le.t
 
module Latin1 :
  T
    with type cp = Cset.Latin1.cp
     and type letter = Cset.Latin1.letter
     and type cset_t = Cset.Latin1.t
