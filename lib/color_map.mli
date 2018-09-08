type t

val make : unit -> t

val flatten : t -> bytes * bytes * int

val split : Cset.t -> t -> unit
