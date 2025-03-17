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
let min = Int.min
let max = Int.max
let compare = Int.compare

module Int = struct
  let[@warning "-32"] hash (x : int) = Hashtbl.hash x

  include Stdlib.Int
end
