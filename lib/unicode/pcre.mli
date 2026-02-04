(** NOTE: Only a subset of the PCRE spec is supported *)

exception Parse_error
exception Not_supported

type flag = [ `CASELESS | `MULTILINE | `ANCHORED | `DOTALL ]

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text of string  (** Text part of splitted string *)
  | Delim of string  (** Delimiter part of splitted string *)
  | Group of int * string
    (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup  (** Unmatched subgroup *)

module type T = sig
  type core
  type re
  type groups

  (** [re ~flags s] creates the regexp [s] using the pcre syntax. *)
  val re : ?flags:flag list -> string -> core

  val re_result :
    ?flags:flag list ->
    string ->
    (core, [ `Not_supported | `Parse_error ]) result

  (** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)
  val regexp : ?flags:flag list -> string -> re

  (** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)
  val extract : rex:re -> string -> string array

  (** Equivalent to {!Core.exec}. *)
  val exec : rex:re -> ?pos:int -> string -> groups

  (** Equivalent to {!Core.Group.get}. *)
  val get_substring : groups -> int -> string

  (** Return the names of named groups. *)
  val names : re -> string array

  (** Return the first matched named group, or raise [Not_found]. Prefer to use
      the non-raising version [get_named_substring_opt] *)
  val get_named_substring : re -> string -> groups -> string

  (** Return the first matched named group, or raise [Not_found]. *)
  val get_named_substring_opt : re -> string -> groups -> string option

  (** Equivalent to {!Core.Group.offset}. *)
  val get_substring_ofs : groups -> int -> int * int

  (** Equivalent to {!Core.execp}. *)
  val pmatch : rex:re -> string -> bool

  val substitute : rex:re -> subst:(string -> string) -> string -> string
  val full_split : ?max:int -> rex:re -> string -> split_result list
  val split : rex:re -> string -> string list
  val quote : string -> string

  (** {2 Deprecated} *)

  type substrings = Group.t
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) :
  T
    with type core = Core.Make(Cset)(Color_map).t
     and type re = Core.Make(Cset)(Color_map).re
     and type groups = Core.Make(Cset)(Color_map).Group.t
