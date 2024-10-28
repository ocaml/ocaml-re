type t =
  | Int of int
  | Tuple of t list
  | Enum of string
  | String of string
  | List of t list
  | Variant of string * t list
  | Record of (string * t) list

let variant x y = Variant (x, y)
let list x = List x
let int x = Int x
let pair x y = Tuple [ x; y ]
let record fields = Record fields
let enum x = Enum x
let string s = String s

let result ok err = function
  | Ok s -> variant "Ok" [ ok s ]
  | Error e -> variant "Error" [ err e ]
;;

let option f = function
  | None -> enum "None"
  | Some s -> variant "Some" [ f s ]
;;
