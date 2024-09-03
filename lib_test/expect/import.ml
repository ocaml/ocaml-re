module Re = Re_private.Re
include Re_private.Import
module Fmt = Re_private.Fmt

let printf = Printf.printf

let t re s =
  let group = Re.exec_opt (Re.compile re) s in
  Format.printf "%a@." (Fmt.opt Re.Group.pp) group
;;
