(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

type t              (* Regular expression *)
type re             (* Compiled regular expression *)
type substrings     (* Match informations *)

(* Compilation and execution of a regular expression *)
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

(* Substring extraction *)
val get : substrings -> int -> string
      (* Raise [Not_found] if the group did not match *)
val get_ofs : substrings -> int -> int * int
      (* Raise [Not_found] if the group did not match *)
val get_all : substrings -> string array
      (* Return the empty string for each group which did not match *)
val get_all_ofs : substrings -> (int * int) array
      (* Return [(-1,-1)] for each group which did not match *)
val test : substrings -> int -> bool
      (* Test whether a group matched *)

(* String expressions (literal match) *)
val str : string -> t
val char : char -> t

(* Basic operations on regular expressions *)
val alt : t list -> t                  (* Alternative *)
val seq : t list -> t                  (* Sequence *)
val empty : t                          (* Match nothing *)
val epsilon : t                        (* Empty word *)
val rep : t -> t                       (* 0 or more matches *)
val rep1 : t -> t                      (* 1 or more matches *)
val repn : t -> int -> int option -> t (* Repeated matches *)
val opt : t -> t                       (* 0 or 1 matches *)

(* String, line, word *)
val bol : t                            (* Beginning of line *)
val eol : t                            (* End of line *)
val bow : t                            (* Beginning of word *)
val eow : t                            (* End of word *)
val bos : t                            (* Beginning of string *)
val eos : t                            (* End of string *)
val leol : t                           (* Last end of line or end of string *)
val start : t                          (* Initial position *)
val stop : t                           (* Final position *)
val word : t -> t                      (* Word *)
val not_boundary : t                   (* Not at a word boundary *)

(* Match semantics *)
val longest : t -> t                   (* Longest match *)
val shortest : t -> t                  (* Shortest match *)
val first : t -> t                     (* First match *)

(* Repeated match modifiers *)
val greedy : t -> t                    (* Greedy *)
val non_greedy : t -> t                (* Non-greedy *)

(* Groups (or submatches) *)
val group : t -> t                     (* Delimit a group *)
val no_group : t -> t                  (* Remove all groups *)
val nest : t -> t
    (* when matching against [nest e], only the group matching in the
       last match of e will be considered as matching *)

(* Character sets *)
val set : string -> t                  (* Any character of the string *)
val rg : char -> char -> t             (* Character ranges *)
val inter : t list -> t                (* Intersection of character sets *)
val diff : t -> t -> t                 (* Difference of character sets *)
val compl : t list -> t                (* Complement of union *)

(* Predefined character sets *)
val any : t                            (* Any character *)
val notnl : t                          (* Any character but a newline *)
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

(* Case modifiers *)
val case : t -> t                      (* Case sensitive matching *)
val no_case : t -> t                   (* Case insensitive matching *)

(****)

(* Internal debugging *)
val print_re : Format.formatter -> re -> unit
