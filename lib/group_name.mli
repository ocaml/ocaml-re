type t = private string

val to_string : t -> string
val of_string : string -> t
val to_dyn : t -> Dyn.t
val pp : t Fmt.t
