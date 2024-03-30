type t

val singleton : bool -> t

val length : t -> int

val set : t -> int -> bool -> unit

val create : int -> bool -> t

val get : t -> int -> bool

val set_all : t -> bool -> unit
