type regexp = Re0.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type groups = Re0.groups

(** Result of a {!Pcre.full_split} *)
type split_result =
  | Text  of string       (** Text part of splitted string *)
  | Delim of string       (** Delimiter part of splitted string *)
  | Group of int * string (** Subgroup of matched delimiter (subgroup_nr, subgroup_str) *)
  | NoGroup               (** Unmatched subgroup *)

val re : ?flags:(flag list) -> string -> Re0.t
(** [re ~flags s] creates the regexp [s] using the pcre syntax. *)

val regexp : ?flags:(flag list) -> string -> regexp
(** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)

val extract : rex:regexp -> string -> string array
(** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)

val exec : rex:regexp -> ?pos:int -> string -> groups
(** Equivalent to {!Re0.exec}. *)

val get_substring : groups -> int -> string
(** Equivalent to {!Re0.Group.get}. *)

val get_substring_ofs : groups -> int -> int * int
(** Equivalent to {!Re0.Group.offset}. *)

val pmatch : rex:regexp -> string -> bool
(** Equivalent to {!Re0.execp}. *)

val substitute : rex:Re0.re -> subst:(string -> string) -> string -> string

val full_split : ?max:int -> rex:regexp -> string -> split_result list

val split : rex:regexp -> string -> string list

val quote : string -> string

(** {2 Deprecated} *)

type substrings = Re0.groups
