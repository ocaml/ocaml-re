type t =
  | Int of int
  | Tuple of t list
  | Enum of string
  | List of t list
  | Variant of string * t list
  | Record of (string * t) list

let variant x y = Variant (x, y)
let list x = List x
let int x = Int x
let pair x y = Tuple [ x; y ]
let record fields = Record fields
