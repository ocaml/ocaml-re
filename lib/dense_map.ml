let make ~size ~f =
  let cache = Array.init size f in
  fun i -> cache.(i)
;;
