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

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

type opt = [ `ICase | `NoSub | `Newline ]

module type T = sig
  type core
  type re

  (** Parsing of a Posix extended regular expression *)
  val re : ?opts:opt list -> string -> core

  val re_result :
    ?opts:opt list -> string -> (core, [ `Not_supported | `Parse_error ]) result

  (** [compile r] is defined as [Core.compile (Core.longest r)] *)
  val compile : core -> re

  (** [compile_pat ?opts regex] compiles the Posix extended regular expression
      [regexp] *)
  val compile_pat : ?opts:opt list -> string -> re
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module Re = Core.Make (Cset) (Color_map)

  type core = Re.t
  type re = Re.re

  module CodePage = Cset.CodePage
  module Categories = CodePage.Categories
  module Posix_class = Posix_class.Make (Cset) (Color_map)
  module Parse_buffer = Posix_class.Parse_buffer

  type elem = Letter of Cset.letter | Set of Re.t

  let ( !! ) = CodePage.of_char
  let ( !^ ) = fun x -> CodePage.from_letter @@ CodePage.of_char x

  let parse newline s =
    let buf = Parse_buffer.create s in
    let accept = Parse_buffer.accept buf in
    let eos () = Parse_buffer.eos buf in
    let test c = Parse_buffer.test buf c in
    let unget () = Parse_buffer.unget buf in
    let get () = Parse_buffer.get buf in
    let rec regexp () = regexp' [ branch () ]
    and regexp' left =
      if accept !!'|' then regexp' (branch () :: left)
      else Re.alt (List.rev left)
    and branch () = branch' []
    and branch' left =
      if eos () || (fst @@ test !!'|') || (fst @@ test !!')') then
        Re.seq (List.rev left)
      else branch' (piece () :: left)
    and piece () =
      let r = atom () in
      if accept !!'*' then Re.rep (Re.nest r)
      else if accept !!'+' then Re.rep1 (Re.nest r)
      else if accept !!'?' then Re.opt r
      else if accept !!'{' then (
        match Parse_buffer.integer buf with
        | Some i ->
          let j = if accept !!',' then Parse_buffer.integer buf else Some i in
          if not (accept !!'}') then raise Parse_error;
          (match j with Some j when j < i -> raise Parse_error | _ -> ());
          Re.repn (Re.nest r) i j
        | None ->
          unget ();
          r)
      else r
    and atom () =
      if accept !!'.' then if newline then Re.notnl else Re.any
      else if accept !!'(' then (
        let r = regexp () in
        if not (accept !!')') then raise Parse_error;
        Re.group r)
      else if accept !!'^' then if newline then Re.bol else Re.bos
      else if accept !!'$' then if newline then Re.eol else Re.eos
      else if accept !!'[' then
        if accept !!'^' then
          Re.diff (Re.compl (bracket [])) (Re.letter @@ CodePage.of_char '\n')
        else Re.alt (bracket [])
      else if accept !!'\\' then (
        if eos () then raise Parse_error;
        match CodePage.from_letter @@ get () with
        | cp
          when CodePage.equal cp !^'|' || CodePage.equal cp !^'('
               || CodePage.equal cp !^')' || CodePage.equal cp !^'*'
               || CodePage.equal cp !^'+' || CodePage.equal cp !^'?'
               || CodePage.equal cp !^'[' || CodePage.equal cp !^'.'
               || CodePage.equal cp !^'^' || CodePage.equal cp !^'$'
               || CodePage.equal cp !^'{' || CodePage.equal cp !^'\\' ->
          Re.letter @@ CodePage.to_letter cp
        | _ -> raise Parse_error)
      else (
        if eos () then raise Parse_error;
        match CodePage.from_letter @@ get () with
        | cp
          when CodePage.equal cp !^'*' || CodePage.equal cp !^'+'
               || CodePage.equal cp !^'?' || CodePage.equal cp !^'{'
               || CodePage.equal cp !^'\\' ->
          raise Parse_error
        | cp -> Re.letter @@ CodePage.to_letter cp)
    and bracket s =
      if s <> [] && accept !!']' then s
      else
        match letter () with
        | Set st -> bracket (st :: s)
        | Letter l ->
          if accept !!'-' then
            if accept !!']' then Re.letter l :: Re.letter !!'-' :: s
            else
              bracket
                (match letter () with
                | Letter l' -> Re.rg l l' :: s
                | Set st' -> Re.letter l :: Re.letter !!'-' :: st' :: s)
          else bracket (Re.letter l :: s)
    and letter () =
      if eos () then raise Parse_error;
      let l = get () in
      if Cset.Codec.equal l !!'[' then
        match Posix_class.parse buf with
        | Some set -> Set set
        | None ->
          if accept !!'.' then (
            if eos () then raise Parse_error;
            let l' = get () in
            if not (accept !!'.') then raise Not_supported;
            if not (accept !!']') then raise Parse_error;
            Letter l')
          else Letter l
      else Letter l
    in
    let res = regexp () in
    if not (eos ()) then raise Parse_error;
    res

  let re ?(opts = []) s =
    let r = parse (List.memq `Newline opts) s in
    let r = if List.memq `ICase opts then Re.no_case r else r in
    let r = if List.memq `NoSub opts then Re.no_group r else r in
    r

  let re_result ?opts s =
    match re ?opts s with
    | s -> Ok s
    | exception Not_supported -> Error `Not_supported
    | exception Parse_error -> Error `Parse_error

  let compile re = Re.compile (Re.longest re)
  let compile_pat ?(opts = []) s = compile (re ~opts s)
end
