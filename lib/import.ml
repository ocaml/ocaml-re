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
let ( < ) x y = Int.compare x y = -1
let ( > ) x y = Int.compare x y = 1
let compare = Int.compare
