type t =
  { s : string
  ; pos : int
  ; len : int
  }

module L : sig
  type nonrec t = t list

  val get_substring : t -> start:int -> stop:int -> string
  val drop_rev : t -> int -> t
end
