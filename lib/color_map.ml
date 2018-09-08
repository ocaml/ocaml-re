type t = Bytes.t

let make () = Bytes.make 257 '\000'

let flatten cm =
  let c = Bytes.create 256 in
  let col_repr = Bytes.create 256 in
  let v = ref 0 in
  Bytes.set c 0 '\000';
  Bytes.set col_repr 0 '\000';
  for i = 1 to 255 do
    if Bytes.get cm i <> '\000' then incr v;
    Bytes.set c i (Char.chr !v);
    Bytes.set col_repr !v (Char.chr i)
  done;
  (c, Bytes.sub col_repr 0 (!v + 1), !v + 1)

let split s cm =
  Cset.iter s ~f:(fun i j ->
      Bytes.set cm i '\001';
      Bytes.set cm (j + 1) '\001';
    )
