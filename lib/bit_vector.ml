type t = bool array

let singleton v = [| v |]

let length t = Array.length t

let set t i a = t.(i) <- a

let get t i = t.(i)

let create len v = Array.make len v

let set_all t v =
  Array.fill t 0 (length t) v
