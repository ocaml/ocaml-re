type t =
  { str : string
  ; mutable pos : int
  }

exception Parse_error

let create str = { str; pos = 0 }
let unget t = t.pos <- t.pos - 1
let junk t = t.pos <- t.pos + 1
let eos t = t.pos = String.length t.str
let test t c = (not (eos t)) && t.str.[t.pos] = c

let test2 t c c' =
  t.pos + 1 < String.length t.str && t.str.[t.pos] = c && t.str.[t.pos + 1] = c'
;;

let accept t c =
  let r = test t c in
  if r then t.pos <- t.pos + 1;
  r
;;

let get t =
  let r = t.str.[t.pos] in
  t.pos <- t.pos + 1;
  r
;;

let accept_s t s' =
  let len = String.length s' in
  t.pos + len <= String.length t.str
  &&
  let i = ref 0 in
  while !i < len && Char.equal t.str.[t.pos + !i] s'.[!i] do
    i := !i + 1
  done;
  if !i = len
  then (
    t.pos <- t.pos + len;
    true)
  else false
;;

let accept_until_before t c =
  match String.index_from_opt t.str t.pos c with
  | None -> None
  | Some pos_before_c ->
    let s = String.sub t.str t.pos (pos_before_c - t.pos) in
    t.pos <- pos_before_c;
    Some s
;;

let rec integer' t i =
  if eos t
  then Some i
  else (
    match get t with
    | '0' .. '9' as d ->
      let i' = (10 * i) + (Char.code d - Char.code '0') in
      if i' < i then raise Parse_error;
      integer' t i'
    | _ ->
      unget t;
      Some i)
;;

let integer t =
  if eos t
  then None
  else (
    match get t with
    | '0' .. '9' as d -> integer' t (Char.code d - Char.code '0')
    | _ ->
      unget t;
      None)
;;
