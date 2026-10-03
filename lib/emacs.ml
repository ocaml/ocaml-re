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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

module Re = Core
open Import

exception Parse_error
exception Not_supported

let emacs_of_name = function
  | "multibyte" -> Some (Re.set "")
  | "nonascii" -> Some (Re.compl [ Re.ascii ])
  | "unibyte" -> Some Re.any
  | name -> Posix_class.of_name name
;;

let parse ~emacs_only s =
  let buf = Parse_buffer.create s in
  let accept = Parse_buffer.accept buf in
  let eos () = Parse_buffer.eos buf in
  let test2 = Parse_buffer.test2 buf in
  let get () = Parse_buffer.get buf in
  let rec regexp () = regexp' [ branch () ]
  and regexp' left =
    if Parse_buffer.accept_s buf {|\||}
    then regexp' (branch () :: left)
    else Re.alt (List.rev left)
  and branch () = branch' true []
  and branch' start left =
    if eos () || test2 '\\' '|' || test2 '\\' ')'
    then Re.seq (List.rev left)
    else (
      let before = Parse_buffer.position buf in
      let r = piece start in
      let next_start =
        start
        &&
        let consumed = Parse_buffer.position buf - before in
        (consumed = 1 && Char.equal s.[before] '^')
        || (consumed = 2 && Char.equal s.[before] '\\' && Char.equal s.[before + 1] '`')
      in
      branch' next_start (r :: left))
  and piece start =
    let before = Parse_buffer.position buf in
    let r = atom start in
    let leading_anchor =
      start
      &&
      let consumed = Parse_buffer.position buf - before in
      (consumed = 1 && Char.equal s.[before] '^')
      || (consumed = 2 && Char.equal s.[before] '\\' && Char.equal s.[before + 1] '`')
    in
    let quantified r = if accept '?' then Re.non_greedy r else r in
    if leading_anchor
    then r
    else if accept '*'
    then quantified (Re.rep r)
    else if accept '+'
    then quantified (Re.rep1 r)
    else if accept '?'
    then quantified (Re.opt r)
    else if Parse_buffer.accept_s buf {|\{|}
    then (
      let rep =
        match Parse_buffer.integer buf with
        | Some i ->
          let j = if accept ',' then Parse_buffer.integer buf else Some i in
          if not (Parse_buffer.accept_s buf {|\}|}) then raise Parse_error;
          (match j with
           | Some j when j < i -> raise Parse_error
           | _ -> ());
          Re.repn (Re.nest r) i j
        | None ->
          if accept ','
          then (
            let j = Parse_buffer.integer buf in
            if not (Parse_buffer.accept_s buf {|\}|}) then raise Parse_error;
            Re.repn (Re.nest r) 0 j)
          else raise Parse_error
      in
      if accept '?' then Re.opt rep else rep)
    else r
  and atom start =
    if accept '.'
    then Re.notnl
    else if accept '^'
    then Re.bol
    else if accept '$'
    then Re.eol
    else if accept '['
    then if accept '^' then Re.compl (bracket []) else Re.alt (bracket [])
    else if accept '\\'
    then
      if accept '('
      then
        if Parse_buffer.accept_s buf "?:"
        then (
          let r = regexp () in
          if not (Parse_buffer.accept_s buf {|\)|}) then raise Parse_error;
          r)
        else if Parse_buffer.test buf '?'
        then raise Not_supported
        else (
          let r = regexp () in
          if not (Parse_buffer.accept_s buf {|\)|}) then raise Parse_error;
          Re.group r)
      else if emacs_only && accept '`'
      then Re.bos
      else if emacs_only && accept '\''
      then Re.eos
      else if accept '='
      then Re.start
      else if accept 'b'
      then Re.alt [ Re.bow; Re.eow ]
      else if emacs_only && accept 'B'
      then Re.not_boundary
      else if emacs_only && accept '<'
      then Re.bow
      else if emacs_only && accept '>'
      then Re.eow
      else if accept 'w'
      then Re.alt [ Re.alnum; Re.char '_' ]
      else if accept 'W'
      then Re.compl [ Re.alnum; Re.char '_' ]
      else (
        if eos () then raise Parse_error;
        match get () with
        | ('*' | '+' | '?' | '[' | ']' | '.' | '^' | '$' | '\\') as c -> Re.char c
        | '0' .. '9' -> raise Not_supported
        | ('s' | 'S' | 'c' | 'C' | '_') when emacs_only -> raise Not_supported
        | c -> Re.char c)
    else (
      if eos () then raise Parse_error;
      match get () with
      | ('*' | '+' | '?') as c -> if start then Re.char c else raise Parse_error
      | c -> Re.char c)
  and bracket s =
    if s <> [] && accept ']'
    then s
    else (
      match char () with
      | `Set st -> bracket (st :: s)
      | `Char c ->
        if accept '-'
        then
          if accept ']'
          then Re.char c :: Re.char '-' :: s
          else (
            match char () with
            | `Char c' ->
              let range = if Char.compare c c' > 0 then Re.set "" else Re.rg c c' in
              bracket (range :: s)
            | `Set st' -> bracket (Re.char c :: Re.char '-' :: st' :: s))
        else bracket (Re.char c :: s))
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if Char.equal c '['
    then (
      match Posix_class.parse emacs_of_name buf with
      | Some set -> `Set set
      | None -> `Char c)
    else `Char c
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res
;;

let re ?(case = true) s =
  let r = parse s ~emacs_only:true in
  if case then r else Re.no_case r
;;

let re_no_emacs ~case s =
  let r = parse s ~emacs_only:false in
  if case then r else Re.no_case r
;;

let re_result ?case s =
  match re ?case s with
  | s -> Ok s
  | exception Not_supported -> Error `Not_supported
  | exception Parse_error -> Error `Parse_error
;;

let compile = Re.compile
let compile_pat ?(case = true) s = compile (re ~case s)
