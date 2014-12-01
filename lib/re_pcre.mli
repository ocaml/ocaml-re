type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type substrings = Re.substrings

val re : ?flags:(flag list) -> string -> Re.t

val regexp : ?flags:(flag list) -> string -> regexp

val extract : rex:regexp -> string -> string array

val exec : rex:regexp -> ?pos:int -> string -> substrings

val get_substring : substrings -> int -> string

val get_substring_ofs : substrings -> int -> int * int

val pmatch : rex:regexp -> string -> bool
