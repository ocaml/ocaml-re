
exception Parse_error

module type T = sig
  type letter
  type t
  val create : string -> t
  val junk : t -> unit
  val unget : t -> unit
  val eos : t -> bool
  val test : t -> letter -> bool * int
  val test2 : t -> letter -> letter -> bool
  val get : t -> letter
  val accept : t -> letter -> bool
  val accept_s : t -> string -> bool
  val integer : t -> int option
end

module Make (Cset : Cset.T) : T with type letter = Cset.letter
