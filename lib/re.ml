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

include Re_core

let cany = [0, 255]

let any = Set cany
let notnl = Set (Cset.diff cany (csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']
let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]
let wordc = alt [alnum; char '_']
let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'F']

let case_insens s =
  Set (Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                       (Cset.offset (-32) (Cset.inter s clower))))

let compile r =
  let r = handle_case case_insens cany false r in
  compile_1 (if anchored r then group r else seq [shortest (rep any); group r])
