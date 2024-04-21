(* Result of a successful match. *)
type t =
  { s : string
  ; marks : Mark_infos.t
  ; pmarks : Pmark.Set.t
  ; gpos : int array
  ; gcount : int
  }

let offset t i =
  match Mark_infos.offset t.marks i with
  | None -> raise Not_found
  | Some (start, stop) -> t.gpos.(start), t.gpos.(stop)
;;

let get t i =
  let p1, p2 = offset t i in
  String.sub t.s p1 (p2 - p1)
;;

let start subs i = fst (offset subs i)
let stop subs i = snd (offset subs i)
let test t i = Mark_infos.test t.marks i
let get_opt t i = if test t i then Some (get t i) else None
let dummy_offset = -1, -1

let all_offset t =
  let res = Array.make t.gcount dummy_offset in
  Mark_infos.iteri t.marks ~f:(fun i start stop ->
    let p1 = t.gpos.(start) in
    let p2 = t.gpos.(stop) in
    res.(i) <- p1, p2);
  res
;;

let dummy_string = ""

let all t =
  let res = Array.make t.gcount dummy_string in
  Mark_infos.iteri t.marks ~f:(fun i start stop ->
    let p1 = t.gpos.(start) in
    let p2 = t.gpos.(stop) in
    res.(i) <- String.sub t.s p1 (p2 - p1));
  res
;;

let pp fmt t =
  let matches =
    let offsets = all_offset t in
    let strs = all t in
    Array.to_list (Array.init (Array.length strs) (fun i -> strs.(i), offsets.(i)))
  in
  let open Format in
  let open Fmt in
  let pp_match fmt (str, (start, stop)) = fprintf fmt "@[(%s (%d %d))@]" str start stop in
  sexp fmt "Group" (list pp_match) matches
;;

let nb_groups t = t.gcount
