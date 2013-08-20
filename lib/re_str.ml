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

type regexp =
  { re : Re.t;
    mutable mtch : Re.re option;
    mutable srch : Re.re option }

let compile_regexp s c =
  { re = Re_emacs.re ~case:(not c) s;
    mtch = None;
    srch = None }

let rec get_mtch re =
  match re.mtch with
    Some r -> r
  | None   -> re.mtch <- Some (Re.compile (Re.seq [Re.start; re.re]));
              get_mtch re

let rec get_srch re =
  match re.srch with
    Some r -> r
  | None   -> re.srch <- Some (Re.compile re.re);
              get_srch re

let state = ref None

let string_match re s p =
  try
    state := Some (Re.exec ~pos:p (get_mtch re) s);
    true
  with Not_found ->
    state := None;
    false

let string_partial_match re s p =
  match
    Re.exec_partial ~pos:p (get_mtch re) s
  with
    `Full     -> string_match re s p
  | `Partial  -> true
  | `Mismatch -> false

let search_forward re s p =
  try
    let res = Re.exec ~pos:p (get_srch re) s in
    state := Some res;
    fst (Re.get_ofs res 0)
  with Not_found ->
    state := None;
    raise Not_found

let rec search_backward re s p =
  try
    let res = Re.exec ~pos:p (get_mtch re) s in
    state := Some res;
    p
  with Not_found ->
    state := None;
    if p = 0 then raise Not_found else
    search_backward re s (p - 1)

let beginning_group i =
  match !state with
    Some m -> fst (Re.get_ofs m i)
  | None   -> raise Not_found

let end_group i =
  match !state with
    Some m -> snd (Re.get_ofs m i)
  | None   -> raise Not_found

let get_len i =
  match !state with
    None   -> 0
  | Some m ->
      try
        let (b, e) = Re.get_ofs m i in
        e - b
      with Not_found ->
        0

let rec repl_length repl p q len =
  if p < len then begin
    if repl.[p] <> '\\' then
      repl_length repl (p + 1) (q + 1) len
    else begin
      let p = p + 1 in
      if p = len then failwith "Str.replace: illegal backslash sequence";
      match repl.[p] with
        '\\' ->
          repl_length repl (p + 1) (q + 1) len
      | '0' .. '9' as c ->
          repl_length
            repl (p + 1) (q + get_len (Char.code c - Char.code '0')) len
      | _ ->
          repl_length repl (p + 1) (q + 2) len
    end
  end else
    q

let rec replace orig repl p res q len =
  if p < len then begin
    let c = repl.[p] in
    if c <> '\\' then begin
      res.[q] <- c;
      replace orig repl (p + 1) res (q + 1) len
    end else begin
      match repl.[p + 1] with
        '\\' ->
          res.[q] <- '\\';
          replace orig repl (p + 2) res (q + 1) len
      | '0' .. '9' as c ->
          let d =
            try
              match !state with
                None ->
                  raise Not_found
              | Some m ->
                  let (b, e) = Re.get_ofs m (Char.code c - Char.code '0') in
                  let d = e - b in
                  if d > 0 then String.blit orig b res q d;
                  d
            with Not_found ->
              0
          in
          replace repl orig (p + 2) res (q + d) len
      | c ->
          res.[q] <- '\\';
          res.[q + 1] <- c;
          replace repl orig (p + 2) res (q + 2) len
    end
  end

let replacement_text repl orig =
  let len = String.length repl in
  let res = String.create (repl_length repl 0 0 len) in
  replace orig repl 0 res 0 (String.length repl);
  res

let quote s =
  let len = String.length s in
  let buf = String.create (2 * len) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match s.[i] with
      '[' | ']' | '*' | '.' | '\\' | '?' | '+' | '^' | '$' as c ->
        buf.[!pos] <- '\\'; buf.[!pos + 1] <- c; pos := !pos + 2
    | c ->
        buf.[!pos] <- c; pos := !pos + 1
  done;
  String.sub buf 0 !pos

let string_before s n = String.sub s 0 n

let string_after s n = String.sub s n (String.length s - n)

let first_chars s n = String.sub s 0 n

let last_chars s n = String.sub s (String.length s - n) n

let regexp e = compile_regexp e false

let regexp_case_fold e = compile_regexp e true

let regexp_string s = compile_regexp (quote s) false

let regexp_string_case_fold s = compile_regexp (quote s) true

let group_beginning n =
  if n < 0 || n >= 10 then invalid_arg "Str.group_beginning" else
  let pos = beginning_group n in
  if pos = -1 then raise Not_found else pos

let group_end n =
  if n < 0 || n >= 10 then invalid_arg "Str.group_end" else
  let pos = end_group n in
  if pos = -1 then raise Not_found else pos

let matched_group n txt =
  let b = group_beginning n and e = group_end n in String.sub txt b (e-b)

let replace_matched repl matched = replacement_text repl matched

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    String.concat "" [string_before text pos;
                      repl_fun text;
                      string_after text (match_end())]
  with Not_found ->
    text

let global_substitute expr repl_fun text =
  let rec replace start last_was_empty =
    try
      let startpos = if last_was_empty then start + 1 else start in
      if startpos > String.length text then raise Not_found;
      let pos = search_forward expr text startpos in
      let end_pos = match_end() in
      let repl_text = repl_fun text in
      String.sub text start (pos-start) ::
      repl_text ::
      replace end_pos (end_pos = pos)
    with Not_found ->
      [string_after text start] in
  String.concat "" (replace 0 false)

let global_replace expr repl text =
  global_substitute expr (replacement_text repl) text
and replace_first expr repl text =
  substitute_first expr (replacement_text repl) text

let bounded_split expr text num =
  let start =
    if string_match expr text 0 then match_end() else 0 in
  let rec split start n =
    if start >= String.length text then [] else
    if n = 1 then [string_after text start] else
      try
        let pos = search_forward expr text start in
        String.sub text start (pos-start) :: split (match_end()) (n-1)
      with Not_found ->
        [string_after text start] in
  split start num

let split expr text = bounded_split expr text 0

let bounded_split_delim expr text num =
  let rec split start n =
    if start > String.length text then [] else
    if n = 1 then [string_after text start] else
      try
        let pos = search_forward expr text start in
        String.sub text start (pos-start) :: split (match_end()) (n-1)
      with Not_found ->
        [string_after text start] in
  if text = "" then [] else split 0 num

let split_delim expr text = bounded_split_delim expr text 0

type split_result = Text of string | Delim of string

let bounded_full_split expr text num =
  let rec split start n =
    if start >= String.length text then [] else
    if n = 1 then [Text(string_after text start)] else
      try
        let pos = search_forward expr text start in
        let s = matched_string text in
        if pos > start then
          Text(String.sub text start (pos-start)) ::
          Delim(s) ::
          split (match_end()) (n-1)
        else
          Delim(s) ::
          split (match_end()) (n-1)
      with Not_found ->
        [Text(string_after text start)] in
  split 0 num

let full_split expr text = bounded_full_split expr text 0
