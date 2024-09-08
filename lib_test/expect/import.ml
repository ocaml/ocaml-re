module Re = Re_private.Re
include Re_private.Import
module Fmt = Re_private.Fmt

let printf = Printf.printf

let t re s =
  let group = Re.exec_opt (Re.compile re) s in
  Format.printf "%a@." (Fmt.opt Re.Group.pp) group
;;

let re_whitespace = Re.Pcre.regexp "[\t ]+"
let re_eol = Re.compile Re.eol
let re_bow = Re.compile Re.bow
let re_eow = Re.compile Re.eow
let strings = Format.printf "[%a]@." Fmt.(list ~pp_sep:(Fmt.lit "; ") Fmt.quoted_string)
let re_empty = Re.Posix.compile_pat ""
