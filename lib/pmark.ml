open Import

module Pmark = struct
  type t = int

  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let r = Atomic.make 1
  let gen () = Atomic.fetch_and_add r 1
  let pp = Format.pp_print_int
end

include Pmark

module Set = struct
  include Set.Make (Pmark)

  let equal x y = Phys_equal.equal x y || equal x y
  let compare x y = if Phys_equal.equal x y then 0 else compare x y
  let to_list = elements
end

let to_dyn = Dyn.int
