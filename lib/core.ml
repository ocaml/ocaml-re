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

open Import

include struct
  let cset = Ast.cset
  let char c = cset (Cset.csingle c)
  let rg c c' = cset (Cset.cseq c c')
  let any = cset Cset.cany
  let notnl = cset Cset.notnl
  let lower = cset Cset.lower
  let upper = cset Cset.upper
  let alpha = cset Cset.alpha
  let digit = cset Cset.cdigit
  let alnum = cset Cset.alnum
  let wordc = cset Cset.wordc
  let ascii = cset Cset.ascii
  let blank = cset Cset.blank
  let cntrl = cset Cset.cntrl
  let graph = cset Cset.graph
  let print = cset Cset.print
  let punct = cset Cset.punct
  let space = cset Cset.space
  let xdigit = cset Cset.xdigit
end

include Ast.Export

let exec_internal name ?(pos = 0) ?(len = -1) ~partial ~groups re s =
  if pos < 0 || len < -1 || pos + len > String.length s then invalid_arg name;
  Compile.match_str ~groups ~partial re s ~pos ~len
;;

let exec ?pos ?len re s =
  match exec_internal "Re.exec" ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> substr
  | _ -> raise Not_found
;;

let exec_opt ?pos ?len re s =
  match exec_internal "Re.exec_opt" ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> Some substr
  | _ -> None
;;

let execp ?pos ?len re s =
  match exec_internal ~groups:false ~partial:false "Re.execp" ?pos ?len re s with
  | Match _substr -> true
  | _ -> false
;;

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false ~partial:true "Re.exec_partial" ?pos ?len re s with
  | Match _ -> `Full
  | Running _ -> `Partial
  | Failed -> `Mismatch
;;

let exec_partial_detailed ?pos ?len re s =
  match
    exec_internal ~groups:true ~partial:true "Re.exec_partial_detailed" ?pos ?len re s
  with
  | Match group -> `Full group
  | Running { no_match_starts_before } -> `Partial no_match_starts_before
  | Failed -> `Mismatch
;;

module Mark = struct
  type t = Pmark.t

  let test (g : Group.t) p = Pmark.Set.mem p g.pmarks
  let all (g : Group.t) = g.pmarks

  module Set = Pmark.Set

  let equal = Pmark.equal
  let compare = Pmark.compare
end

type split_token =
  [ `Text of string
  | `Delim of Group.t
  ]

module Rseq = struct
  let all ?(pos = 0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.all";
    (* index of the first position we do not consider.
       !pos < limit is an invariant *)
    let limit =
      match len with
      | None -> String.length s
      | Some l ->
        if l < 0 || pos + l > String.length s then invalid_arg "Re.all";
        pos + l
    in
    (* iterate on matches. When a match is found, search for the next
       one just after its end *)
    let rec aux pos on_match () =
      if pos > limit
      then Seq.Nil (* no more matches *)
      else (
        match
          Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
        with
        | Match substr ->
          let p1, p2 = Group.offset substr 0 in
          if on_match && p1 = pos && p1 = p2
          then (* skip empty match right after a match *)
            aux (pos + 1) false ()
          else (
            let pos = if p1 = p2 then p2 + 1 else p2 in
            Seq.Cons (substr, aux pos (p1 <> p2)))
        | Running _ | Failed -> Seq.Nil)
    in
    aux pos false
  ;;

  let matches ?pos ?len re s : _ Seq.t =
    all ?pos ?len re s |> Seq.map (fun sub -> Group.get sub 0)
  ;;

  let split_full ?(pos = 0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.split";
    let limit =
      match len with
      | None -> String.length s
      | Some l ->
        if l < 0 || pos + l > String.length s then invalid_arg "Re.split";
        pos + l
    in
    (* i: start of delimited string
       pos: first position after last match of [re]
       limit: first index we ignore (!pos < limit is an invariant) *)
    let pos0 = pos in
    let rec aux state i pos () =
      match state with
      | `Idle when pos > limit ->
        (* We had an empty match at the end of the string *)
        assert (i = limit);
        Seq.Nil
      | `Idle ->
        (match
           Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
         with
         | Match substr ->
           let p1, p2 = Group.offset substr 0 in
           let pos = if p1 = p2 then p2 + 1 else p2 in
           let old_i = i in
           let i = p2 in
           if old_i = p1 && p1 = p2 && p1 > pos0
           then (* Skip empty match right after a delimiter *)
             aux state i pos ()
           else if p1 > pos0
           then (
             (* string does not start by a delimiter *)
             let text = String.sub s old_i (p1 - old_i) in
             let state = `Yield (`Delim substr) in
             Seq.Cons (`Text text, aux state i pos))
           else Seq.Cons (`Delim substr, aux state i pos)
         | Running _ -> Seq.Nil
         | Failed ->
           if i < limit
           then (
             let text = String.sub s i (limit - i) in
             (* yield last string *)
             Seq.Cons (`Text text, aux state limit pos))
           else Seq.Nil)
      | `Yield x -> Seq.Cons (x, aux `Idle i pos)
    in
    aux `Idle pos pos
  ;;

  let split ?pos ?len re s : _ Seq.t =
    let seq = split_full ?pos ?len re s in
    let rec filter seq () =
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (`Delim _, tl) -> filter tl ()
      | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter tl)
    in
    filter seq
  ;;

  let split_delim ?pos ?len re s : _ Seq.t =
    let seq = split_full ?pos ?len re s in
    let rec filter ~delim seq () =
      match seq () with
      | Seq.Nil -> if delim then Seq.Cons ("", fun () -> Seq.Nil) else Seq.Nil
      | Seq.Cons (`Delim _, tl) ->
        if delim
        then Seq.Cons ("", fun () -> filter ~delim:true tl ())
        else filter ~delim:true tl ()
      | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter ~delim:false tl)
    in
    filter ~delim:true seq
  ;;
end

module Rlist = struct
  let list_of_seq (s : 'a Seq.t) : 'a list =
    Seq.fold_left (fun l x -> x :: l) [] s |> List.rev
  ;;

  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> list_of_seq
  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> list_of_seq
  let split_full ?pos ?len re s = Rseq.split_full ?pos ?len re s |> list_of_seq
  let split ?pos ?len re s = Rseq.split ?pos ?len re s |> list_of_seq
  let split_delim ?pos ?len re s = Rseq.split_delim ?pos ?len re s |> list_of_seq
end

module Gen = struct
  type 'a gen = unit -> 'a option

  let gen_of_seq (s : 'a Seq.t) : 'a gen =
    let r = ref s in
    fun () ->
      match !r () with
      | Seq.Nil -> None
      | Seq.Cons (x, tl) ->
        r := tl;
        Some x
  ;;

  let split ?pos ?len re s : _ gen = Rseq.split ?pos ?len re s |> gen_of_seq
  let split_full ?pos ?len re s : _ gen = Rseq.split_full ?pos ?len re s |> gen_of_seq
  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> gen_of_seq
  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> gen_of_seq
end

let replace ?(pos = 0) ?len ?(all = true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit =
    match len with
    | None -> String.length s
    | Some l ->
      if l < 0 || pos + l > String.length s then invalid_arg "Re.replace";
      pos + l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos on_match =
    if pos <= limit
    then (
      match
        Compile.match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos)
      with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        if pos = p1 && p1 = p2 && on_match
        then (
          (* if we matched an empty string right after a match,
             we must manually advance by 1 *)
          if p2 < limit then Buffer.add_char buf s.[p2];
          iter (p2 + 1) false)
        else (
          (* add string between previous match and current match *)
          Buffer.add_substring buf s pos (p1 - pos);
          (* what should we replace the matched group with? *)
          let replacing = f substr in
          Buffer.add_string buf replacing;
          if all
          then
            (* if we matched an empty string, we must manually advance by 1 *)
            iter
              (if p1 = p2
               then (
                 (* a non char could be past the end of string. e.g. $ *)
                 if p2 < limit then Buffer.add_char buf s.[p2];
                 p2 + 1)
               else p2)
              (p1 <> p2)
          else Buffer.add_substring buf s p2 (limit - p2))
      | Running _ -> ()
      | Failed -> Buffer.add_substring buf s pos (limit - pos))
  in
  iter pos false;
  Buffer.contents buf
;;

let replace_string ?pos ?len ?all re ~by s = replace ?pos ?len ?all re s ~f:(fun _ -> by)

let witness t =
  let rec witness (t : Ast.no_case) =
    match t with
    | Set c -> String.make 1 (Cset.to_char (Cset.pick c))
    | Sequence xs -> String.concat "" (List.map ~f:witness xs)
    | Ast (Alternative (x :: _)) -> witness x
    | Ast (Alternative []) -> assert false
    | Repeat (r, from, _to) ->
      let w = witness r in
      let b = Buffer.create (String.length w * from) in
      for _i = 1 to from do
        Buffer.add_string b w
      done;
      Buffer.contents b
    | Ast (No_group r | Sem (_, r) | Sem_greedy (_, r)) -> witness r
    | Nest r | Pmark (_, r) | Group (_, r) -> witness r
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str -> ""
  in
  witness (Ast.handle_case false t)
;;

module Seq = Rseq
module Group = Group

(** {2 Deprecated functions} *)

let split_full_seq = Seq.split_full
let split_seq = Seq.split
let matches_seq = Seq.matches
let all_seq = Seq.all

type 'a gen = 'a Gen.gen

let all_gen = Gen.all
let matches_gen = Gen.matches
let split_gen = Gen.split
let split_full_gen = Gen.split_full

type substrings = Group.t

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

(**********************************)

(*
   Information about the previous character:
   - does not exists
   - is a letter
   - is not a letter
   - is a newline
   - is last newline

   Beginning of word:
   - previous is not a letter or does not exist
   - current is a letter or does not exist

   End of word:
   - previous is a letter or does not exist
   - current is not a letter or does not exist

   Beginning of line:
   - previous is a newline or does not exist

   Beginning of buffer:
   - previous does not exist

   End of buffer
   - current does not exist

   End of line
   - current is a newline or does not exist
*)

(*
   Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

type groups = Group.t

include Rlist
module List = Rlist

include struct
  open Compile

  type nonrec re = re

  let compile = compile
  let pp_re = pp_re
  let print_re = print_re
  let group_names = group_names
  let group_count = group_count
end
