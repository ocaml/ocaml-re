open Import

module Array = struct
  type nonrec t = Bytes.t (* instead of int array so the gc doesn't scan it *)

  let words = 8
  let[@inline] length t = Bytes.length t / words
  let[@inline] unsafe_get t i = Int64.to_int (Bytes.get_int64_ne t (i * words))
  let[@inline] unsafe_set t i x = Bytes.set_int64_ne t (i * words) (Int64.of_int x)
  let[@inline] make_absent len = Bytes.make (len * words) '\255'
  let clear t = Bytes.fill t 0 (Bytes.length t) '\255'

  let fold_left t ~init ~f =
    let init = ref init in
    for i = 0 to length t - 1 do
      init := f !init (unsafe_get t i)
    done;
    !init
  ;;
end

type t =
  { mutable table : Array.t (* sized to powers of two *)
  ; mutable size : int
  }

let[@inline] should_grow t = Array.length t.table <= t.size * 2
let absent = -1

let () =
  let x = Array.make_absent 1 in
  assert (Array.unsafe_get x 0 = absent)
;;

let create () = { size = 0; table = Array.make_absent 0 }
let[@inline] index_of_offset slots index i = (index + i) land (slots - 1)

let clear t =
  t.size <- 0;
  Array.clear t.table
;;

let add t x =
  let hash = Int.hash x in
  let slots = Array.length t.table in
  let index = hash land (slots - 1) in
  let inserting = ref true in
  let i = ref 0 in
  while !inserting do
    let idx = index_of_offset slots index !i in
    let elem = Array.unsafe_get t.table idx in
    if elem = absent
    then (
      Array.unsafe_set t.table idx x;
      inserting := false)
    else incr i
  done;
  t.size <- t.size + 1
;;

let resize t =
  let old_table = t.table in
  let slots = Array.length old_table in
  let table = Array.make_absent (if slots = 0 then 1 else slots lsl 1) in
  t.table <- table;
  for i = 0 to slots - 1 do
    let elem = Array.unsafe_get old_table i in
    if elem <> absent then add t elem
  done
;;

let add t x =
  if should_grow t then resize t;
  add t x
;;

let[@inline] is_empty t = t.size = 0

let mem t x =
  (not (is_empty t))
  &&
  let hash = Int.hash x in
  let slots = Array.length t.table in
  let index = hash land (slots - 1) in
  let i = ref 0 in
  let found = ref false in
  while (not !found) && !i < slots do
    let idx = index_of_offset slots index !i in
    let elem = Array.unsafe_get t.table idx in
    if Int.equal elem x
    then found := true
    else if Int.equal elem absent
    then i := slots
    else incr i
  done;
  !found
;;

let pp fmt { table; size } =
  let table =
    Array.fold_left table ~init:[] ~f:(fun acc i -> if i = absent then acc else i :: acc)
    |> List.rev
    |> Stdlib.Array.of_list
  in
  let table fmt () = Fmt.sexp fmt "table" Fmt.(array int) table in
  let size fmt () = Fmt.sexp fmt "size" Fmt.int size in
  Format.fprintf fmt "%a@.%a@." table () size ()
;;
