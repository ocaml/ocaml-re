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
  module Set = Set.Make (Pmark)

  let[@warning "-32"] to_list x =
    let open Set in
    to_seq x |> List.of_seq
  ;;

  include Set
end

let to_dyn = Dyn.int
