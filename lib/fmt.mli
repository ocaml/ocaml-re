type formatter := Format.formatter
type 'a t = formatter -> 'a -> unit

val pp_str_list : string list t
val sexp : formatter -> string -> 'a t -> 'a -> unit
val str : string t
val optint : int option t
val int : int t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val list : ?pp_sep:unit t -> 'a t -> 'a list t
val to_to_string : 'a t -> 'a -> string
