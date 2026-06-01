(** Color maps exists to provide an optimization for the regex engine. The fact
    that some characters are entirely equivalent for some regexes means that we
    can use them interchangeably.

    A color map assigns a color to every character in our character set. The
    automaton then operates on colors rather than characters, thus reducing the
    number of transitions, and so the size of the transition tables and the
    number of partial derivatives to compute. *)
type t

module Repr : sig
  type t

  val repr : t -> Cset.c -> char
  val length : t -> int
end

module Table : sig
  type t

  val get_char : t -> Cset.c -> char
  val get : t -> char -> Cset.c
  val translate_colors : t -> Cset.t -> Cset.t
  val to_dyn : t -> Dyn.t
end

val make : unit -> t
val flatten : t -> Table.t * Repr.t
val split : t -> Cset.t -> unit
