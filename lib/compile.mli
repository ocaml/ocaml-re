type re

type match_info =
  | Match of Group.t
  | Failed
  | Running of { no_match_starts_before : int }

val match_str
  :  groups:bool
  -> partial:bool
  -> re
  -> string
  -> pos:int
  -> len:int
  -> match_info

val compile : Ast.t -> re
val group_count : re -> int
val group_names : re -> (string * int) list

(* CR rgrinberg: deprecate one of these *)
val print_re : re Fmt.t
val pp_re : re Fmt.t
