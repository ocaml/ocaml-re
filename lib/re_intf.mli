module type Char = sig
  type t
  val code : t -> int
  val chr : int -> t
  val category : t -> int
  val of_char : char -> t
end

module type String = sig
  type t
  module Char : Char
  val of_string : string -> t
  val create : int -> t
  val length : t -> int
  val make : int -> Char.t -> t
  val sub : t -> int -> int -> t
  val get : t -> int -> Char.t
  val set : t -> int -> Char.t -> unit
end

module type S = sig
  type string
  type char

  type t
  (** Regular expression *)

  type re
  (** Compiled regular expression *)

  type substrings
  (** Match informations *)

  (** {2 Compilation and execution of a regular expression} *)

  val compile : t -> re
  val exec :
    ?pos:int ->    (* Default: 0 *)
    ?len:int ->    (* Default: -1 (until end of string) *)
    re -> string -> substrings
  val execp :
    ?pos:int ->    (* Default: 0 *)
    ?len:int ->    (* Default: -1 (until end of string) *)
    re -> string -> bool
  val exec_partial :
    ?pos:int ->    (* Default: 0 *)
    ?len:int ->    (* Default: -1 (until end of string) *)
    re -> string -> [ `Full | `Partial | `Mismatch ]

  (** {2 Substring extraction} *)

  val get : substrings -> int -> string
  (** Raise [Not_found] if the group did not match *)

  val get_ofs : substrings -> int -> int * int
  (** Raise [Not_found] if the group did not match *)

  val get_all : substrings -> string array
  (** Return the empty string for each group which did not match *)

  val get_all_ofs : substrings -> (int * int) array
  (** Return [(-1,-1)] for each group which did not match *)

  val test : substrings -> int -> bool
  (** Test whether a group matched *)

  (** {2 String expressions (literal match)} *)

  val str : string -> t
  val char : char -> t

  (** {2 Basic operations on regular expressions} *)

  val alt : t list -> t
  (** Alternative *)

  val seq : t list -> t
  (** Sequence *)

  val empty : t
  (** Match nothing *)

  val epsilon : t
  (** Empty word *)

  val rep : t -> t
  (** 0 or more matches *)

  val rep1 : t -> t
  (** 1 or more matches *)

  val repn : t -> int -> int option -> t
  (** Repeated matches *)

  val opt : t -> t
  (** 0 or 1 matches *)

  (** {2 String, line, word} *)

  val bol : t
  (** Beginning of line *)

  val eol : t
  (** End of line *)

  val bow : t
  (** Beginning of word *)

  val eow : t
  (** End of word *)

  val bos : t
  (** Beginning of string *)

  val eos : t
  (** End of string *)

  val leol : t
  (** Last end of line or end of string *)

  val start : t
  (** Initial position *)

  val stop : t
  (** Final position *)

  val word : t -> t
  (** Word *)

  val not_boundary : t
  (** Not at a word boundary *)

  val whole_string : t -> t
  (** Only matches the whole string *)

  (** {2 Match semantics} *)

  val longest : t -> t
  (** Longest match *)

  val shortest : t -> t
  (** Shortest match *)

  val first : t -> t
  (** First match *)

  (** {2 Repeated match modifiers} *)

  val greedy : t -> t
  (** Greedy *)

  val non_greedy : t -> t
  (** Non-greedy *)

  (** {2 Groups (or submatches)} *)

  val group : t -> t
  (** Delimit a group *)

  val no_group : t -> t
  (** Remove all groups *)

  val nest : t -> t
  (** when matching against [nest e], only the group matching in the
      last match of e will be considered as matching *)

  (** {2 Character sets} *)

  val set : string -> t
  (** Any character of the string *)

  val rg : char -> char -> t
  (** Character ranges *)

  val inter : t list -> t
  (** Intersection of character sets *)

  val diff : t -> t -> t
  (** Difference of character sets *)

  val compl : t list -> t
  (** Complement of union *)

  (** {2 Predefined character sets} *)

  val any : t
  (** Any character *)

  val notnl : t
  (** Any character but a newline *)

  val alnum : t
  val alpha : t
  val ascii : t
  val blank : t
  val cntrl : t
  val digit : t
  val graph : t
  val lower : t
  val print : t
  val punct : t
  val space : t
  val upper : t
  val xdigit : t

  (** {2 Case modifiers} *)

  val case : t -> t
  (** Case sensitive matching *)

  val no_case : t -> t
  (** Case insensitive matching *)

  (****)

  (** {2 Internal debugging}  *)

  val print_re : Format.formatter -> re -> unit
end
