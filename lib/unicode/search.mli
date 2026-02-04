module type T = sig
  type re

  val all : ?pos:int -> ?len:int -> re -> string -> Group.t Seq.t
  val matches : ?pos:int -> ?len:int -> re -> string -> string Seq.t

  val split_full :
    ?pos:int ->
    ?len:int ->
    re ->
    string ->
    [> `Delim of Group.t | `Text of string ] Seq.t

  val split : ?pos:int -> ?len:int -> re -> string -> string Seq.t
  val split_delim : ?pos:int -> ?len:int -> re -> string -> string Seq.t
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) :
  T with type re = Compile.Make(Cset)(Color_map).re
