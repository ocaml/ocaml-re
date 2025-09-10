let make : size:int -> f:(int -> 'a @ portable) -> (int -> 'a @ contended) @ portable =
  fun ~size ~f ->
  let cache =
    (* SAFETY: Interface requires that we never put anything nonportable in here *)
    Obj.magic_portable (Array.init size f)
  in
  fun i ->
    (((* SAFETY: we never mutate this array. *) Obj.magic_uncontended cache).(i))
;;
