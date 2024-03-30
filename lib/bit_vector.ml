type t =
  { len : int
  ; bits : Bytes.t
  }
let byte s i = Char.code (Bytes.unsafe_get s i)

let low_mask = Array.init 9 (fun i -> (1 lsl i) - 1)
let set_byte s i x = Bytes.unsafe_set s i (Char.chr x)

let create len b =
  let initv = if b then 255 else 0 in
  let q = len lsr 3 in
  let r = len land 7 in
  if r = 0 then
    { len ; bits = Bytes.make q (Char.chr initv) }
  else begin
    let s = Bytes.make (q + 1) (Char.chr initv) in
    set_byte s q (initv land low_mask.(r));
    { len ; bits = s }
  end

let singleton v = create 1 v

let length t = t.len

let unsafe_set v n b =
  let i = n lsr 3 in
  let c = byte v.bits i in
  let mask = 1 lsl (n land 7) in
  set_byte v.bits i (if b then c lor mask else c land (lnot mask))

let set v n b =
  if n < 0 || n >= v.len then invalid_arg "Bitv.set";
  unsafe_set v n b

let unsafe_get v n =
  let i = n lsr 3 in
  (byte v.bits i) land (1 lsl (n land 7)) > 0

let get v n =
  if n < 0 || n >= v.len then invalid_arg "Bitv.get";
  unsafe_get v n

let reset_zero t =
  for i = 0 to Bytes.length t.bits - 1 do
    Bytes.set t.bits i '\000'
  done
