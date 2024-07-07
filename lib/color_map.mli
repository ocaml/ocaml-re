(* Color maps exists to provide an optimization for the regex engine. The fact
   that some characters are entirely equivalent for some regexes means that we
   can use them interchangeably.

   A color map assigns a color to every character in our character set. Any two
   characters with the same color will be treated equivalently by the automaton.
*)
type t

module Repr : sig
  type t

  val repr : t -> Cset.c -> char
end

module Table : sig
  type t

  val get_char : t -> Cset.c -> char
  val get : t -> char -> Cset.c
end

val make : unit -> t
val flatten : t -> Table.t * Repr.t * int
val split : Cset.t -> t -> unit
