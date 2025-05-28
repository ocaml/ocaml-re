module DLS = struct
  let new_key f = ref (f())
  let set x y = x := y
  let get x = !x
end
