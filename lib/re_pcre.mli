type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type groups = Re.groups

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text  of string       (** Text part of splitted string *)
  | Delim of string       (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup               (** Unmatched subgroup *)

val re : ?flags:(flag list) -> string -> Re.t

val regexp : ?flags:(flag list) -> string -> regexp

val extract : rex:regexp -> string -> string array

val exec : rex:regexp -> ?pos:int -> string -> groups

val get_substring : groups -> int -> string

val get_substring_ofs : groups -> int -> int * int

val pmatch : rex:regexp -> string -> bool

val substitute : rex:Re.re -> subst:(string -> string) -> string -> string

val full_split : ?max:int -> rex:regexp -> string -> split_result list

val split : rex:regexp -> string -> string list

val quote : string -> string

(** {2 Deprecated} *)

type substrings = Re.groups
