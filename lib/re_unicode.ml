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

let cany = [0, 0x10ffff]

let rg a b =
  if a <= b then Set [a, b] else Set [b, a]

let any = Set cany

let unicode_rg (a, b) =
  assert (0 <= a && a <= b);
  let lo x i = 0x80 lor ((x lsr (6 * i)) land 0x3f) in
  let r = ref [] in
  if a <= 0x7f then begin
    let b = min b 0x7f in
    r := [rg a b]
  end;
  if a <= 0x7ff then begin
    let b = min b 0x7ff in
    let hi x = 0xc0 lor ((x lsr 6) land 0x1f) in
    let a0 = lo a 0 and a1 = hi a in
    let b0 = lo b 0 and b1 = hi b in
    r := seq [rg a1 b1; rg a0 b0] :: !r
  end;
  if a <= 0xffff then begin
    let b = min b 0xffff in
    let hi x = 0xe0 lor ((x lsr 12) land 0xf) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = hi b in
    r := seq [rg a2 b2; rg a1 b1; rg a0 b0] :: !r
  end;
  if a <= 0x1fffff then begin
    let b = min b 0x1fffff in
    let hi x = 0xf0 lor ((x lsr 16) land 0x7) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = lo a 2 and a3 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = lo b 2 and b3 = hi b in
    r := seq [rg a3 b3; rg a2 b2; rg a1 b1; rg a0 b0] :: !r
  end;
  !r

let rec handle_unicode r =
  match r with
    Set s ->
      (* let s = Cset.diff s (Cset.seq 0xd800 0xdfff) in (\* remove surrogates *\) *)
      alt (List.concat (List.map unicode_rg s))
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

(* Binary search in the array [a].  It is assumed that [a] (which has elements
   of type int * int * int, is sorted so that if i < j, and [a1, b1, _ =
   a.(i)], [a2, b2, _ = a.(j)], then [a1 <= b1 < a2 < b2].  It returns the
   unique triple [a, b, d] as above such that [a <= c <= b] (if one exists),
   or if none exists, the smallest such interval with [c < a], or if none
   exists, raises [Not_found]. *)
let find_foldcase c a =
  assert (Array.length a > 0);
  let rec loop imin imax =
    let imid = imin + (imax - imin) / 2 in
    let (lo, hi, _) = a.(imid) in
    if c < lo then
      if imid = imin then
        a.(imid)
      else
        loop imin (imid - 1)
    else if hi < c then
      if imid = imax then
        raise Not_found
      else
        loop (imid + 1) imax
    else
      a.(imid)
  in
  loop 0 (Array.length a - 1)

(* Closes the characters set [s] under the equivalent relation of unicode
   simple folding. *)
let case_insens s =
  let s = ref s in
  let rec add c1 c2 =
    if c1 <= c2 then
      match try `Ok (find_foldcase c1 Unicode_groups.foldcase) with Not_found -> `Not_found with
        | `Ok (a, b, d) ->
            if c1 < a then
              add a c2
            else
              let cx = min c2 b in
              let c1d = c1 + d in
              let c2d = cx + d in
              if not (Cset.mem_range (c1d, c2d) !s) then begin
                s := Cset.union (Cset.seq c1d c2d) !s;
                add c1d c2d
              end;
              add (cx + 1) c2
        | `Not_found ->
            ()
  in
  List.iter (fun (c1, c2) -> add c1 c2) !s;
  !s

let compile r =
  let r = if anchored r then group r else seq [shortest (rep any); group r] in
  let r = handle_case case_insens cany false r in
  let r = handle_unicode r in
  compile_1 r

let char c = Set (Cset.single c)

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
  Set (Cset.diff cany (Cset.single 11))

let alnum =
  Set (Cset.union Unicode_groups.alphabetic Unicode_groups.decimal_number)

let wordc =
  let sl =
    [ Unicode_groups.alphabetic;
      Unicode_groups.mark;
      Unicode_groups.decimal_number;
      Unicode_groups.connector_punctuation;
      Unicode_groups.join_control ]
  in
  Set (List.fold_left Cset.union [] sl)

let alpha =
  Set Unicode_groups.alphabetic

let ascii =
  Set [0, 127]

let blank =
  Set (Cset.add 0x9 Unicode_groups.space_separator)

let cntrl =
  Set Unicode_groups.control

let digit =
  Set Unicode_groups.decimal_number

let graph =
  let sl =
    [ Unicode_groups.white_space;
      Unicode_groups.control;
      Unicode_groups.surrogate;
      Unicode_groups.unassigned ]
  in
  Set (List.fold_left Cset.diff cany sl)

let upper =
  Set Unicode_groups.uppercase

let lower =
  Set Unicode_groups.lowercase

let print =
  let sl =
    [ Unicode_groups.white_space;
      Unicode_groups.control;
      Unicode_groups.surrogate;
      Unicode_groups.unassigned;
      Unicode_groups.white_space;
      [0x9, 0x9] ]
  in
  Set (List.fold_left Cset.diff cany sl)

let punct =
  Set Unicode_groups.punctuation

let space =
  Set Unicode_groups.white_space

let xdigit =
  Set Unicode_groups.hex_digit
