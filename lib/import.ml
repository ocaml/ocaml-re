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
let min = Int.min
let max = Int.max
let compare = Int.compare

module Option = struct
  include Option

  module Unboxed : sig
    type 'a t

    val none : 'a t
    val some : 'a -> 'a t
    val is_none : 'a t -> bool
    val is_some : 'a t -> bool
    val value_exn : 'a t -> 'a
    val to_option : 'a t -> 'a option
    val iter : 'a t -> f:('a -> unit) -> unit
  end = struct
    type 'a t = Obj.t

    let some x = Obj.repr x
    let none = Obj.repr (Sys.opaque_identity (-1))
    let phys_equal = Stdlib.( == )
    let is_none t = phys_equal t none
    let is_some t = not (phys_equal t none)

    let value_exn t =
      if is_none t then invalid_arg "Option.Unboxed.value_exn called on None";
      Obj.obj t
    ;;

    let to_option t = if is_none t then None else Some (value_exn t)
    let iter t ~f = if is_none t then () else f (value_exn t)
  end
end

module Int = struct
  let[@warning "-32"] hash (x : int) = Hashtbl.hash x

  include Stdlib.Int
end
