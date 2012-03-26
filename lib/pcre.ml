(*
   Copyright (C) 2012 Thomas Gazagnaire <thomas@ocamlpro.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

type regexp = Re.re

let regexp ?(flags = []) pat =
  let opts = List.map (function
    | `CASELESS -> `Caseless
  ) flags in
  Re_perl.compile_pat ~opts pat

let extract ~rex s =
  Re.get_all (Re.exec rex s)

let exec ~rex ?pos s =
  Re.exec rex ?pos s

let get_substring s i =
  Re.get s i

let get_substring_ofs s i =
  Re.get_ofs s i

let pmatch ~rex s =
  Re.execp rex s

(* From PCRE *)
let string_unsafe_sub s ofs len =
  let r = String.create len in
  String.unsafe_blit s ofs r 0 len;
  r

let quote s =
  let len = String.length s in
  let buf = String.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      String.unsafe_set buf !pos '\\';
      incr pos;
      String.unsafe_set buf !pos c; incr pos
    | c -> String.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub buf 0 !pos

(* XXX: quick hack, untested *)
let qreplace ~pat ~templ str =
  let r = Re_perl.compile_pat pat in
  let ss = Re.exec r str in
  let ofs = Re.get_all_ofs ss in
  let b = Buffer.create (String.length str) in
  let prev = ref 0 in
  for i = 0 to Array.length ofs - 1 do
    let orig, off = ofs.(i) in
    Buffer.add_substring b str !prev (orig - !prev);
    Buffer.add_string b templ;
    prev := orig + off;
  done;
  if !prev <> String.length str then
    Buffer.add_substring b str !prev (String.length str - !prev);
  Buffer.contents b
