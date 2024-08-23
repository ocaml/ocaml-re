module List = Stdlib.ListLabels

module Poly = struct
  let equal = ( = )
end

let ( = ) = Int.equal
