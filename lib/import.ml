module List = Stdlib.ListLabels

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
