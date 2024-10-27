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

exception Parse_error
exception Not_supported

let by_code f c c' =
  let c = Char.code c in
  let c' = Char.code c' in
  Char.chr (f c c')
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
  and branch () = branch' []
  and branch' left =
    if eos () || test2 '\\' '|' || test2 '\\' ')'
    then Re.seq (List.rev left)
    else branch' (piece () :: left)
  and piece () =
    let r = atom () in
    if accept '*'
    then Re.rep r
    else if accept '+'
    then Re.rep1 r
    else if accept '?'
    then Re.opt r
    else r
  and atom () =
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
      then (
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
        | c -> if emacs_only then raise Parse_error else Re.char c)
    else (
      if eos () then raise Parse_error;
      match get () with
      | '*' | '+' | '?' -> raise Parse_error
      | c -> Re.char c)
  and bracket s =
    if s <> [] && accept ']'
    then s
    else (
      let c = char () in
      if accept '-'
      then
        if accept ']'
        then Re.char c :: Re.char '-' :: s
        else (
          let c' = char () in
          let c' = by_code Int.max c c' in
          bracket (Re.rg c c' :: s))
      else bracket (Re.char c :: s))
  and char () =
    if eos () then raise Parse_error;
    get ()
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
