(*
   RE - A regular expression library

   Copyright (C) 2014 Nicolas Ojeda Bar
   email: n.oje.bar@gmail.com

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

include Re_core

type uchar = int

let unicode_rg (a, b) =
  let lo x i = 0x80 lor ((x lsr (6 * i)) land 0x3f) in
  let rg a b = Set (cseq a b) in
  let r = ref [] in
  let a, b = if a <= b then a, b else b, a in
  if a <= 0x7f then begin
    let b = min b 0x7f in
    r := [rg (Char.chr a) (Char.chr b)]
  end;
  if a <= 0x7ff then begin
    let b = min b 0x7ff in
    let hi x = 0xc0 lor ((x lsr 6) land 0x1f) in
    let a0 = lo a 0 and a1 = hi a in
    let b0 = lo b 0 and b1 = hi b in
    r := Sequence [rg (Char.chr a1) (Char.chr b1);
                   rg (Char.chr a0) (Char.chr b0)] :: !r
  end;
  if a <= 0xffff then begin
    let b = min b 0xffff in
    let hi x = 0xe0 lor ((x lsr 12) land 0xf) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = hi b in
    r := Sequence [rg (Char.chr a2) (Char.chr b2);
                   rg (Char.chr a1) (Char.chr b1);
                   rg (Char.chr a0) (Char.chr b0)] :: !r
  end;
  if a <= 0x1fffff then begin
    let b = min b 0x1fffff in
    let hi x = 0xf0 lor ((x lsr 16) land 0x7) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = lo a 2 and a3 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = lo b 2 and b3 = hi b in
    r := Sequence [rg (Char.chr a3) (Char.chr b3);
                   rg (Char.chr a2) (Char.chr b2);
                   rg (Char.chr a1) (Char.chr b1);
                   rg (Char.chr a0) (Char.chr b0)] :: !r
  end;
  alt !r

let rec handle_unicode r =
  match r with
    Set s ->
      let s = Cset.diff s (Cset.seq 0xd800 0xdbff) in (* remove surrogates *)
      alt (List.map unicode_rg s)
  | Sequence l ->
      Sequence (List.map handle_unicode l)
  | Alternative l ->
      Alternative (List.map handle_unicode l)
  | Repeat (r, i, j) ->
      Repeat (handle_unicode r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop ->
      r
  | Sem (k, r) ->
      Sem (k, handle_unicode r)
  | Sem_greedy (k, r) ->
      Sem_greedy (k, handle_unicode r)
  | Group r ->
      Group (handle_unicode r)
  | No_group r ->
      No_group (handle_unicode r)
  | Nest r ->
      Nest (handle_unicode r)
  | Case _ | No_case _ | Intersection _ | Complement _ | Difference _ ->
      assert false

let case_insens s =
  Set s

let any_byte = Set [0, 255]

let compile r =
  let r = handle_case case_insens [0, 0x10ffff] false r in
  let r = handle_unicode r in
  compile_1 (if anchored r then group r else seq [shortest (rep any_byte); group r])

let char c =
  Set (Cset.single c)

(** {2 Character sets} *)

(* let set str = *)
(*   let s = ref [] in *)
(*   let d = Uutf.decoder ~encoding:`UTF_8 (`String str) in *)
(*   let rec loop () = *)
(*     match Uutf.decode d with *)
(*     | `Uchar c -> *)
(*       s := c :: !s; *)
(*       loop () *)
(*     | `End -> *)
(*       alt (List.rev_map uchar !s) *)
(*     | `Malformed _ -> *)
(*       invalid_arg "Re_utf8.set" *)
(*     | `Await -> *)
(*       assert false *)
(*   in *)
(*   loop () *)

let rg a b =
  if a <= b then Set [a, b] else Set [b, a]

let any =
  rg 0 0x10ffff

let all r s =
  all r s

let exec r s =
  exec r s

let execp r s =
  execp r s

let exec_partial r s =
  exec_partial r s

let all_gen r s =
  all_gen r s

let matches r s =
  matches r s

let matches_gen r s =
  matches_gen r s

let split r s =
  split r s

let split_gen r s =
  split_gen r s

let split_full r s =
  split_full r s

let split_full_gen r s =
  split_full_gen r s

let replace ?all r ~f s =
  replace ?all r ~f s

let notnl =
  diff any (char 11)

let alnum =
  let s = Cset.union Unicode_groups.alphabetic Unicode_groups.decimal_number in
  alt (List.map (fun (a, b) -> rg a b) s)

let wordc =
  let sl =
    [ Unicode_groups.alphabetic;
      Unicode_groups.mark;
      Unicode_groups.decimal_number;
      Unicode_groups.connector_punctuation;
      Unicode_groups.join_control ]
  in
  let s = List.fold_left Cset.union [] sl in
  alt (List.map (fun (a, b) -> rg a b) s)

let alpha =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.alphabetic)

let blank =
  let s = Cset.add 0x9 Unicode_groups.space_separator in
  alt (List.map (fun (a, b) -> rg a b) s)

let cntrl =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.control)

let digit =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.decimal_number)

let graph =
  let sl =
    [ Unicode_groups.white_space;
      Unicode_groups.control;
      Unicode_groups.surrogate;
      Unicode_groups.unassigned ]
  in
  let s = List.fold_left Cset.diff [0, 0x10ffff] sl in
  alt (List.map (fun (a, b) -> rg a b) s)

let upper =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.uppercase)

let lower =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.lowercase)

let print =
  let sl =
    [ Unicode_groups.white_space;
      Unicode_groups.control;
      Unicode_groups.surrogate;
      Unicode_groups.unassigned;
      Unicode_groups.white_space;
      [0x9, 0x9] ]
  in
  let s = List.fold_left Cset.diff [0, 0x10ffff] sl in
  alt (List.map (fun (a, b) -> rg a b) s)

let punct =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.punctuation)

let space =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.white_space)

let xdigit =
  alt (List.map (fun (a, b) -> rg a b) Unicode_groups.hex_digit)

(* val case : t -> t *)
(* (\** Case sensitive matching *\) *)

(* val no_case : t -> t *)
(** Case insensitive matching *)
