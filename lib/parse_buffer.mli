type t

exception Parse_error

val create : string -> t
val junk : t -> unit
val position : t -> int

(** Advance by a nonnegative number of bytes within the input. Like [junk],
    this does not check bounds. *)
val advance : t -> int -> unit

val unget : t -> unit
val eos : t -> bool
val test : t -> char -> bool
val test2 : t -> char -> char -> bool
val get : t -> char
val accept : t -> char -> bool
val accept_s : t -> string -> bool
val accept_until_before : t -> char -> string option
val integer : t -> int option
