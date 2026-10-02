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

(*
   What we could (should?) do:
   - a* ==> longest ((shortest (no_group a)* ), a | ())  (!!!)
   - abc understood as (ab)c
   - "((a?)|b)" against "ab" should not bind the first subpattern to anything

   Note that it should be possible to handle "(((ab)c)d)e" efficiently
*)
module Re = Core

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

let parse ~newline ~bre s =
  let buf = Parse_buffer.create s in
  let accept = Parse_buffer.accept buf in
  let eos () = Parse_buffer.eos buf in
  let test c = Parse_buffer.test buf c in
  let test2 = Parse_buffer.test2 buf in
  let unget () = Parse_buffer.unget buf in
  let get () = Parse_buffer.get buf in
  let branch_end () =
    if bre then test2 '\\' '|' || test2 '\\' ')' else test '|' || test ')'
  in
  let rec regexp () = regexp' [ branch () ]
  and regexp' left =
    if if bre then Parse_buffer.accept_s buf "\\|" else accept '|'
    then regexp' (branch () :: left)
    else Re.alt (List.rev left)
  and branch () = branch' true true []
  and branch' anchor_ok start left =
    if eos () || branch_end ()
    then Re.seq (List.rev left)
    else (
      let before = Parse_buffer.position buf in
      let r = piece anchor_ok start in
      let next_start =
        start
        &&
        let consumed = Parse_buffer.position buf - before in
        consumed = 1 && Char.equal s.[before] '^'
      in
      branch' false next_start (r :: left))
  and piece anchor_ok start =
    let before = Parse_buffer.position buf in
    let r = atom anchor_ok start in
    let leading_anchor =
      bre
      && anchor_ok
      && start
      && Parse_buffer.position buf - before = 1
      && Char.equal s.[before] '^'
    in
    if leading_anchor
    then r
    else if accept '*'
    then Re.rep (Re.nest r)
    else if (not bre) && accept '+'
    then Re.rep1 (Re.nest r)
    else if (not bre) && accept '?'
    then Re.opt r
    else if if bre then Parse_buffer.accept_s buf "\\{" else accept '{'
    then (
      match Parse_buffer.integer buf with
      | Some i ->
        let j = if accept ',' then Parse_buffer.integer buf else Some i in
        let closed = if bre then Parse_buffer.accept_s buf "\\}" else accept '}' in
        if not closed then raise Parse_error;
        (match j with
         | Some j when j < i -> raise Parse_error
         | _ -> ());
        Re.repn (Re.nest r) i j
      | None ->
        if bre
        then raise Parse_error
        else (
          unget ();
          r))
    else r
  and atom anchor_ok start =
    if accept '.'
    then
      if newline
      then Re.diff Re.notnl (Re.char '\000')
      else Re.diff Re.any (Re.char '\000')
    else if (not bre) && accept '('
    then (
      let r = regexp () in
      if not (accept ')') then raise Parse_error;
      Re.group r)
    else if accept '^'
    then
      if (not bre) || anchor_ok then if newline then Re.bol else Re.bos else Re.char '^'
    else if accept '$'
    then
      if (not bre) || eos () || test2 '\\' ')' || test2 '\\' '|'
      then if newline then Re.eol else Re.eos
      else Re.char '$'
    else if accept '['
    then
      if accept '^'
      then (
        let r = Re.compl (bracket []) in
        if newline then Re.diff r (Re.char '\n') else r)
      else Re.alt (bracket [])
    else if accept '\\'
    then (
      if eos () then raise Parse_error;
      match get () with
      | '(' when bre ->
        let r = regexp () in
        if not (Parse_buffer.accept_s buf "\\)") then raise Parse_error;
        Re.group r
      | '0' .. '9' when bre -> raise Not_supported
      | ( '|'
        | '('
        | ')'
        | '*'
        | '+'
        | '?'
        | '['
        | ']'
        | '.'
        | '^'
        | '$'
        | '{'
        | '}'
        | '\\' ) as c -> Re.char c
      | _ -> raise Parse_error)
    else (
      if eos () then raise Parse_error;
      match get () with
      | '*' -> if bre && start then Re.char '*' else raise Parse_error
      | ('+' | '?' | '{' | '}') as c when bre -> Re.char c
      | '+' | '?' | '{' | '\\' -> raise Parse_error
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
          else
            bracket
              (match char () with
               | `Char c' -> Re.rg c c' :: s
               | `Set st' -> Re.char c :: Re.char '-' :: st' :: s)
        else bracket (Re.char c :: s))
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '['
    then (
      match Posix_class.parse Posix_class.of_name buf with
      | Some set -> `Set set
      | None ->
        if accept '.'
        then (
          if eos () then raise Parse_error;
          let c = get () in
          if not (accept '.') then raise Not_supported;
          if not (accept ']') then raise Parse_error;
          `Char c)
        else if accept '='
        then (
          if eos () then raise Parse_error;
          let c = get () in
          if not (accept '=') then raise Not_supported;
          if not (accept ']') then raise Parse_error;
          `Char c)
        else `Char c)
    else `Char c
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res
;;

type opt =
  [ `ICase
  | `NoSub
  | `Newline
  | `Bre
  ]

let re ?(opts = []) s =
  let r = parse ~newline:(List.memq `Newline opts) ~bre:(List.memq `Bre opts) s in
  let r = if List.memq `ICase opts then Re.no_case r else r in
  let r = if List.memq `NoSub opts then Re.no_group r else r in
  r
;;

let re_result ?opts s =
  match re ?opts s with
  | s -> Ok s
  | exception Not_supported -> Error `Not_supported
  | exception Parse_error -> Error `Parse_error
;;

let compile re = Re.compile (Re.longest re)
let compile_pat ?(opts = []) s = compile (re ~opts s)
