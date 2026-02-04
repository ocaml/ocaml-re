(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  linking exception.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* Modified by Jerome.Vouillon@pps.jussieu.fr for integration in RE *)

(* $Id: re_str.ml,v 1.3 2002/07/03 15:47:54 vouillon Exp $ *)

module type T = sig
  (** The type of compiled regular expressions. *)
  type regexp

  (** Compile a regular expression. The syntax for regular expressions is the
      same as in Gnu Emacs. The special characters are [$^.*+?[]]. The following
      constructs are recognized:
      - [.     ] matches any character except newline
      - [*     ] (postfix) matches the previous expression zero, one or several
        times
      - [+     ] (postfix) matches the previous expression one or several times
      - [?     ] (postfix) matches the previous expression once or not at all
      - [[..]  ] character set; ranges are denoted with [-], as in [[a-z]]; an
        initial [^], as in [[^0-9]], complements the set
      - [^     ] matches at beginning of line
      - [$     ] matches at end of line
      - [\|    ] (infix) alternative between two expressions
      - [\(..\)] grouping and naming of the enclosed expression
      - [\1    ] the text matched by the first [\(...\)] expression ([\2] for
        the second expression, etc)
      - [\b    ] matches word boundaries
      - [\     ] quotes special characters. *)
  val regexp : string -> regexp

  (** Same as [regexp], but the compiled expression will match text in a
      case-insensitive way: uppercase and lowercase letters will be considered
      equivalent. *)
  val regexp_case_fold : string -> regexp

  (** [Str.quote s] returns a regexp string that matches exactly [s] and nothing
      else. *)
  val quote : string -> string

  (** [Str.regexp_string s] returns a regular expression that matches exactly
      [s] and nothing else. *)
  val regexp_string : string -> regexp

  (** [Str.regexp_string_case_fold] is similar to [Str.regexp_string], but the
      regexp matches in a case-insensitive way. *)
  val regexp_string_case_fold : string -> regexp

  (** {2 String matching and searching} *)

  (** [string_match r s start] tests whether the characters in [s] starting at
      position [start] match the regular expression [r]. The first character of
      a string has position [0], as usual. *)
  val string_match : regexp -> string -> int -> bool

  (** [search_forward r s start] searches the string [s] for a substring
      matching the regular expression [r]. The search starts at position [start]
      and proceeds towards the end of the string. Return the position of the
      first character of the matched substring, or raise [Not_found] if no
      substring matches. *)
  val search_forward : regexp -> string -> int -> int

  (** Same as [search_forward], but the search proceeds towards the beginning of
      the string. *)
  val search_backward : regexp -> string -> int -> int

  (** Similar to [string_match], but succeeds whenever the argument string is a
      prefix of a string that matches. This includes the case of a true complete
      match. *)
  val string_partial_match : regexp -> string -> int -> bool

  (** [matched_string s] returns the substring of [s] that was matched by the
      latest [string_match], [search_forward] or [search_backward]. The user
      must make sure that the parameter [s] is the same string that was passed
      to the matching or searching function. *)
  val matched_string : string -> string

  (** [match_beginning ()] returns the position of the first character of the
      substring that was matched by [string_match], [search_forward] or
      [search_backward]. *)
  val match_beginning : unit -> int

  (** [match_end ()] returns the position of the character following the last
      character of the substring that was matched by [string_match],
      [search_forward] or [search_backward]. *)
  val match_end : unit -> int

  (** [matched_group n s] returns the substring of [s] that was matched by the
      [n]th group [\(...\)] of the regular expression during the latest
      [string_match], [search_forward] or [search_backward]. The user must make
      sure that the parameter [s] is the same string that was passed to the
      matching or searching function. [matched_group n s] raises [Not_found] if
      the [n]th group of the regular expression was not matched. This can happen
      with groups inside alternatives [\|], options [?] or repetitions [*]. For
      instance, the empty string will match [\(a\)*], but [matched_group 1 ""]
      will raise [Not_found] because the first group itself was not matched. *)
  val matched_group : int -> string -> string

  (** [group_beginning n] returns the position of the first character of the
      substring that was matched by the [n]th group of the regular expression.
      Raises [Not_found] if the [n]th group of the regular expression was not
      matched. *)
  val group_beginning : int -> int

  (** [group_end n] returns the position of the character following the last
      character of the matched substring. Raises [Not_found] if the [n]th group
      of the regular expression was not matched. *)
  val group_end : int -> int

  (** {2 Replacement} *)

  (** [global_replace regexp templ s] returns a string identical to [s], except
      that all substrings of [s] that match [regexp] have been replaced by
      [templ]. The replacement template [templ] can contain [\1], [\2], etc;
      these sequences will be replaced by the text matched by the corresponding
      group in the regular expression. [\0] stands for the text matched by the
      whole regular expression. *)
  val global_replace : regexp -> string -> string -> string

  (** Same as [global_replace], except that only the first substring matching
      the regular expression is replaced. *)
  val replace_first : regexp -> string -> string -> string

  (** [global_substitute regexp subst s] returns a string identical to [s],
      except that all substrings of [s] that match [regexp] have been replaced
      by the result of function [subst]. The function [subst] is called once for
      each matching substring, and receives [s] (the whole text) as argument. *)
  val global_substitute : regexp -> (string -> string) -> string -> string

  (** Same as [global_substitute], except that only the first substring matching
      the regular expression is replaced. *)
  val substitute_first : regexp -> (string -> string) -> string -> string

  (** [replace_matched repl s] returns the replacement text [repl] in which
      [\1], [\2], etc. have been replaced by the text matched by the
      corresponding groups in the most recent matching operation. [s] must be
      the same string that was matched during this matching operation. *)
  val replace_matched : string -> string -> string

  (** {2 Splitting} *)

  (** [split r s] splits [s] into substrings, taking as delimiters the
      substrings that match [r], and returns the list of substrings. For
      instance, [split (regexp "[ \t]+") s] splits [s] into blank-separated
      words. An occurrence of the delimiter at the beginning and at the end of
      the string is ignored. *)
  val split : regexp -> string -> string list

  (** Same as [split], but splits into at most [n] substrings, where [n] is the
      extra integer parameter. *)
  val bounded_split : regexp -> string -> int -> string list

  (** Same as [split], but occurrences of the delimiter at the beginning and at
      the end of the string are recognized and returned as empty strings in the
      result. For instance, [split_delim (regexp " ") " abc "] returns
      [[""; "abc"; ""]], while [split] with the same arguments returns
      [["abc"]]. *)
  val split_delim : regexp -> string -> string list

  (** Same as [bounded_split] and [split_delim], but occurrences of the
      delimiter at the beginning and at the end of the string are recognized and
      returned as empty strings in the result. For instance,
      [split_delim (regexp " ") " abc "] returns [[""; "abc"; ""]], while
      [split] with the same arguments returns [["abc"]]. *)
  val bounded_split_delim : regexp -> string -> int -> string list

  type split_result = Text of string | Delim of string

  (** Same as [split_delim], but returns the delimiters as well as the
      substrings contained between delimiters. The former are tagged [Delim] in
      the result list; the latter are tagged [Text]. For instance,
      [full_split (regexp "[{}]") "{ab}"] returns
      [[Delim "{"; Text "ab"; Delim "}"]]. *)
  val full_split : regexp -> string -> split_result list

  (** Same as [split_delim] and [bounded_split_delim], but returns the
      delimiters as well as the substrings contained between delimiters. The
      former are tagged [Delim] in the result list; the latter are tagged
      [Text]. For instance, [full_split (regexp "[{}]") "{ab}"] returns
      [[Delim "{"; Text "ab"; Delim "}"]]. *)
  val bounded_full_split : regexp -> string -> int -> split_result list

  (** {2 Extracting substrings} *)

  (** [string_before s n] returns the substring of all characters of [s] that
      precede position [n] (excluding the character at position [n]). *)
  val string_before : string -> int -> string

  (** [string_after s n] returns the substring of all characters of [s] that
      follow position [n] (including the character at position [n]). *)
  val string_after : string -> int -> string

  (** [first_chars s n] returns the first [n] characters of [s]. This is the
      same function as [string_before]. *)
  val first_chars : string -> int -> string

  (** [last_chars s n] returns the last [n] characters of [s]. *)
  val last_chars : string -> int -> string  
end

module Make (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t)  = struct

module Ast_ = Ast.Make(Cset)(Color_map)
module Ast = Ast_.Export


module Compile = Compile.Make(Cset)(Color_map)
module Core = Core.Make(Cset)(Color_map)
module Emacs = Emacs.Make(Cset)(Color_map)


include struct
  open Core

  let exec = exec
  let exec_partial = exec_partial
end

type regexp =
  { mtch : Compile.re Lazy.t
  ; srch : Compile.re Lazy.t
  }

let compile_regexp s c =
  let re = Emacs.re_no_emacs ~case:(not c) s in
  { mtch = lazy (Compile.compile (Ast.seq [ Ast.start; re ]))
  ; srch = lazy (Compile.compile re)
  }
;;


let state = Domain.DLS.new_key (fun () -> None)

let string_match re s p =
  match exec ~pos:p (Lazy.force re.mtch) s with
  | res ->
    Domain.DLS.set state (Some res);
    true
  | exception Not_found ->
    Domain.DLS.set state None;
    false
;;

let string_partial_match re s p =
  match exec_partial ~pos:p (Lazy.force re.mtch) s with
  | `Full -> string_match re s p
  | `Partial -> true
  | `Mismatch -> false
;;

let search_forward re s p =
  match exec ~pos:p (Lazy.force re.srch) s with
  | res ->
    Domain.DLS.set state (Some res);
    fst (Group.offset res 0)
  | exception Not_found ->
    Domain.DLS.set state None;
    raise Not_found
;;

let rec search_backward re s p =
  match exec ~pos:p (Lazy.force re.mtch) s with
  | res ->
    Domain.DLS.set state (Some res);
    p
  | exception Not_found ->
    Domain.DLS.set state None;
    if p = 0 then raise Not_found else search_backward re s (p - 1)
;;

let valid_group n =
  n >= 0
  && n < 10
  &&
  match Domain.DLS.get state with
  | None -> false
  | Some m -> n < Group.nb_groups m
;;

let offset_group i =
  match Domain.DLS.get state with
  | Some m -> Group.offset m i
  | None -> raise Not_found
;;

let group_len i =
  match offset_group i with
  | b, e -> e - b
  | exception Not_found -> 0
;;

let rec repl_length repl p q len =
  if p < len
  then
    if repl.[p] <> '\\'
    then repl_length repl (p + 1) (q + 1) len
    else (
      let p = p + 1 in
      if p = len then failwith "Str.replace: illegal backslash sequence";
      let q =
        match repl.[p] with
        | '\\' -> q + 1
        | '0' .. '9' as c -> q + group_len (Char.code c - Char.code '0')
        | _ -> q + 2
      in
      repl_length repl (p + 1) q len)
  else q
;;

let rec replace orig repl p res q len =
  if p < len
  then (
    let c = repl.[p] in
    if c <> '\\'
    then (
      Bytes.set res q c;
      replace orig repl (p + 1) res (q + 1) len)
    else (
      match repl.[p + 1] with
      | '\\' ->
        Bytes.set res q '\\';
        replace orig repl (p + 2) res (q + 1) len
      | '0' .. '9' as c ->
        let d =
          let group = Char.code c - Char.code '0' in
          match offset_group group with
          | exception Not_found -> 0
          | b, e ->
            let d = e - b in
            if d > 0 then String.blit orig b res q d;
            d
        in
        replace orig repl (p + 2) res (q + d) len
      | c ->
        Bytes.set res q '\\';
        Bytes.set res (q + 1) c;
        replace orig repl (p + 2) res (q + 2) len))
;;

let replacement_text repl orig =
  let len = String.length repl in
  let res = Bytes.create (repl_length repl 0 0 len) in
  replace orig repl 0 res 0 (String.length repl);
  Bytes.unsafe_to_string res
;;

let quote s =
  let len = String.length s in
  let buf = Buffer.create (2 * len) in
  for i = 0 to len - 1 do
    match s.[i] with
    | ('[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$') as c ->
      Buffer.add_char buf '\\';
      Buffer.add_char buf c
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf
;;

let string_before s n = String.sub s 0 n
let string_after s n = String.sub s n (String.length s - n)
let first_chars s n = String.sub s 0 n
let last_chars s n = String.sub s (String.length s - n) n
let regexp e = compile_regexp e false
let regexp_case_fold e = compile_regexp e true
let regexp_string s = compile_regexp (quote s) false
let regexp_string_case_fold s = compile_regexp (quote s) true

let group_beginning n =
  if not (valid_group n) then invalid_arg "Str.group_beginning";
  let pos = fst (offset_group n) in
  if pos = -1 then raise Not_found else pos
;;

let group_end n =
  if not (valid_group n) then invalid_arg "Str.group_end";
  let pos = snd (offset_group n) in
  if pos = -1 then raise Not_found else pos
;;

let matched_group n txt =
  let b, e = offset_group n in
  String.sub txt b (e - b)
;;

let replace_matched repl matched = replacement_text repl matched

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    String.concat
      ""
      [ string_before text pos; repl_fun text; string_after text (match_end ()) ]
  with
  | Not_found -> text
;;

let global_substitute expr repl_fun text =
  let rec replace accu start last_was_empty =
    let startpos = if last_was_empty then start + 1 else start in
    if startpos > String.length text
    then string_after text start :: accu
    else (
      match search_forward expr text startpos with
      | pos ->
        let end_pos = match_end () in
        let repl_text = repl_fun text in
        replace
          (repl_text :: String.sub text start (pos - start) :: accu)
          end_pos
          (end_pos = pos)
      | exception Not_found -> string_after text start :: accu)
  in
  String.concat "" (List.rev (replace [] 0 false))
;;

let global_replace expr repl text = global_substitute expr (replacement_text repl) text
and replace_first expr repl text = substitute_first expr (replacement_text repl) text

let search_forward_progress re s p =
  let pos = search_forward re s p in
  if match_end () > p
  then pos
  else if p < String.length s
  then search_forward re s (p + 1)
  else raise Not_found
;;

let bounded_split expr text num =
  let start = if string_match expr text 0 then match_end () else 0 in
  let rec split accu start n =
    if start >= String.length text
    then accu
    else if n = 1
    then string_after text start :: accu
    else (
      match search_forward_progress expr text start with
      | pos -> split (String.sub text start (pos - start) :: accu) (match_end ()) (n - 1)
      | exception Not_found -> string_after text start :: accu)
  in
  List.rev (split [] start num)
;;

let split expr text = bounded_split expr text 0

let bounded_split_delim expr text num =
  let rec split accu start n =
    if start > String.length text
    then accu
    else if n = 1
    then string_after text start :: accu
    else (
      match search_forward_progress expr text start with
      | pos -> split (String.sub text start (pos - start) :: accu) (match_end ()) (n - 1)
      | exception Not_found -> string_after text start :: accu)
  in
  if text = "" then [] else List.rev (split [] 0 num)
;;

let split_delim expr text = bounded_split_delim expr text 0

type split_result =
  | Text of string
  | Delim of string

let bounded_full_split expr text num =
  let rec split accu start n =
    if start >= String.length text
    then accu
    else if n = 1
    then Text (string_after text start) :: accu
    else (
      match search_forward_progress expr text start with
      | pos ->
        let s = matched_string text in
        if pos > start
        then
          split
            (Delim s :: Text (String.sub text start (pos - start)) :: accu)
            (match_end ())
            (n - 1)
        else split (Delim s :: accu) (match_end ()) (n - 1)
      | exception Not_found -> Text (string_after text start) :: accu)
  in
  List.rev (split [] 0 num)
;;

let full_split expr text = bounded_full_split expr text 0
end