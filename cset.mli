
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
