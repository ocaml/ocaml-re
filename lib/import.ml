module List = struct
  let[@warning "-32"] rec equal ~eq l1 l2 = match l1, l2 with
    | [], [] -> true
    | [], _::_ | _::_, [] -> false
    | x::xs, y::ys -> if eq x y then equal ~eq xs ys else false

  let[@warning "-32"] rec compare ~cmp l1 l2 = match l1, l2 with
    | [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | x::xs, y::ys ->
        let r = cmp x y in
        if r = 0 then compare ~cmp xs ys else r

  include Stdlib.ListLabels
end

module Poly = struct
  let equal = ( = )
  let compare = compare
end

module Phys_equal = struct
  let equal = ( == )
end

let ( = ) = Int.equal
let ( == ) = [ `Use_phys_equal ]
let ( < ) (x : int) (y : int) = x < y
let ( > ) (x : int) (y : int) = x > y
let min (x : int) (y : int) = if x <= y then x else y
let max (x : int) (y : int) = if x >= y then x else y
let compare = Int.compare

module Int = struct
  let[@warning "-32"] hash (x : int) = Hashtbl.hash x
  let[@warning "-32"] max (x : int) (y : int) = if x >= y then x else y

  include Stdlib.Int
end
