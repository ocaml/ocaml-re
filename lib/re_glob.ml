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

exception Parse_error

let gany = Re.compl [Re.char '/']
let notdot = Re.compl [Re.char '.'; Re.char '/']
let dot = Re.char '.'

type loc = Beg | BegAny | Mid

let beg_start =
  Re.opt (Re.seq [notdot; Re.rep gany])

let beg_start' =
  Re.seq [notdot; Re.rep gany]

let glob_parse ?anchored init s =
  let i = ref 0 in
  let l = String.length s in
  let eos () = !i = l in
  let test c = not (eos ()) && s.[!i] = c in
  let accept c = let r = test c in if r then incr i; r in
  let get () = let r = s.[!i] in incr i; r in

  let rec expr () = expr' init []
  and expr' beg left =
    if eos () then
      match beg with
        Mid | Beg -> Re.seq (List.rev left)
      | BegAny -> Re.seq (List.rev (beg_start :: left))
    else
      let (piec, beg) = piece beg in expr' beg (piec :: left)
  and piece beg =
    if accept '*' then begin
      if beg <> Mid then
        (Re.seq [], BegAny)
      else
        (Re.rep gany, Mid)
    end else if accept '?' then
      (begin match beg with
         Beg    -> notdot
       | BegAny -> Re.seq [notdot; Re.rep gany]
       | Mid    -> gany
       end,
       Mid)
    else if accept '[' then begin
      let set =
        if accept '^' || accept '!' then
          Re.compl (bracket [])
        else
          Re.alt (bracket [])
      in
      (begin match beg with
         Beg    -> Re.inter [notdot; set]
       | BegAny -> Re.alt [Re.seq [beg_start; Re.inter [notdot; set]];
                           Re.seq [beg_start'; Re.inter [dot; set]]]
       | Mid    -> Re.inter [gany; set]
       end,
       Mid)
    end else
      let c = char () in
      ((if beg <> BegAny then
          Re.char c
        else if c = '.' then
          Re.seq [beg_start'; Re.char c]
        else
          Re.seq [beg_start; Re.char c]),
       if c = '/' then init else Mid)
  and bracket s =
    if s <> [] && accept ']' then s else begin
      let c = char () in
      if accept '-' then begin
        if accept ']' then Re.char c :: Re.char '-' :: s else begin
          let c' = char () in
          bracket (Re.rg c c' :: s)
        end
      end else
        bracket (Re.char c :: s)
    end
  and char () =
    ignore (accept '\\');
    if eos () then raise Parse_error;
    get ()
  in
  let res = expr () in
  if anchored = None then res else Re.whole_string res

let rec mul l l' =
  List.flatten (List.map (fun s -> List.map (fun s' -> s ^ s') l') l)

let explode str =
  let l = String.length str in
  let rec expl inner s i acc beg =
    if i >= l then begin
      if inner then raise Parse_error;
      (mul beg [String.sub str s (i - s)], i)
    end else
    match str.[i] with
      '\\' -> expl inner s (i + 2) acc beg
    | '{' ->
        let (t, i') = expl true (i + 1) (i + 1) [] [""] in
        expl inner i' i' acc
          (mul beg (mul [String.sub str s (i - s)] t))
    | ',' when inner ->
        expl inner (i + 1) (i + 1)
          (mul beg [String.sub str s (i - s)] @ acc) [""]
    | '}' when inner ->
        (mul beg [String.sub str s (i - s)] @ acc, i + 1)
    | _ ->
        expl inner s (i + 1) acc beg
  in
  List.rev (fst (expl false 0 0 [] [""]))

let glob' ?anchored nodot s =
  glob_parse ?anchored (if nodot then Beg else Mid) s
let glob ?anchored s = glob' ?anchored true s
let globx' ?anchored nodot s =
  Re.alt (List.map (glob' ?anchored nodot) (explode s))
let globx ?anchored s = globx' ?anchored true s
