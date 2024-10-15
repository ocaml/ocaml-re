type re

module Stream : sig
  type t

  type 'a feed =
    | Ok of 'a
    | No_match

  val create : re -> t
  val feed : t -> string -> pos:int -> len:int -> t feed
  val finalize : t -> string -> pos:int -> len:int -> bool

  module Group : sig
    type stream := t
    type t

    module Match : sig
      type t

      val get : t -> int -> string option
      val test_mark : t -> Pmark.t -> bool
    end

    val create : stream -> t
    val feed : t -> string -> pos:int -> len:int -> t feed
    val finalize : t -> string -> pos:int -> len:int -> Match.t feed
    val no_match_starts_before : t -> int
  end
end

type match_info =
  | Match of Group.t
  | Failed
  | Running of { no_match_starts_before : int }

val match_str_no_bounds
  :  groups:bool
  -> partial:bool
  -> re
  -> string
  -> pos:int
  -> len:int
  -> match_info

val match_str
  :  groups:bool
  -> partial:bool
  -> re
  -> string
  -> pos:int
  -> len:int
  -> match_info

val match_str_p : re -> string -> pos:int -> len:int -> bool
val compile : Ast.t -> re
val group_count : re -> int
val group_names : re -> (string * int) list
val pp_re : re Fmt.t
