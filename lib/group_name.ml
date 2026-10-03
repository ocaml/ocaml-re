type t = string

let to_string x = x
let of_string x = x
let to_dyn = Dyn.string
let pp = Format.pp_print_string
