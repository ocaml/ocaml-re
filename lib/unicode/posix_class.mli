module type T = sig
  type core
  type letter
  module Parse_buffer : Parse_buffer.T with type letter = letter

  val names : string list
  val of_name : string -> core
  val parse : Parse_buffer.t -> core option
end

module Make (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) :
  T
    with type core = Core.Make(Cset)(Color_map).t
     and type letter = Cset.letter
