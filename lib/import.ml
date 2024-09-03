module List = Stdlib.ListLabels

module Poly = struct
  let equal = ( = )
  let compare = compare
end

let ( = ) = Int.equal
let compare = Int.compare
