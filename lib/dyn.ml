type t =
  | Int of int
  | Pair of t * t
  | Enum of string
  | List of t list
  | Constructor of string * t list
  | Record of (string * t) list

let variant x y = Constructor (x, y)
let list x = List x
let int x = Int x
let pair x y = Pair (x, y)
let record fields = Record fields
