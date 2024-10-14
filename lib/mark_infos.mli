(** store mark information for groups in an array *)
type t

val make : (int * int) list -> t
val offset : t -> int -> (int * int) option
val test : t -> int -> bool
val iteri : t -> f:(int -> int -> int -> unit) -> unit

module Offset : sig
  type t

  val is_present : t -> bool
  val get_no_check : t -> int
end

val start_offset : t -> int -> Offset.t
val stop_offset : t -> int -> Offset.t
