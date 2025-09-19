type 'a t = 'a ref

let make x = ref x
let get x = !x
let set atomic v = atomic := v

let fetch_and_add atomic n =
  let v = !atomic in
  atomic := v + n;
  v
