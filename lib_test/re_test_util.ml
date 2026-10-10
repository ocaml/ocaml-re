let commas n =
  let s = Int.to_string n in
  let len = String.length s in
  let b = Buffer.create (len + (len / 3)) in
  String.iteri
    (fun i c ->
       if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
       Buffer.add_char b c)
    s;
  Buffer.contents b
;;
