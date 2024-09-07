(* Result of a successful match. *)
type t =
  { s : string
  ; marks : Mark_infos.t
  ; pmarks : Pmark.Set.t
  ; gpos : int array
  ; gcount : int
  }

let create s ~gcount ~gpos marks pmarks = { s; gcount; gpos; marks; pmarks }

let offset_opt t i =
  Mark_infos.offset t.marks i
  |> Option.map (fun (start, stop) -> t.gpos.(start), t.gpos.(stop))
;;

let or_not_found = function
  | None -> raise Not_found
  | Some s -> s
;;

let offset t i = offset_opt t i |> or_not_found

let get_opt t i =
  offset_opt t i |> Option.map (fun (p1, p2) -> String.sub t.s p1 (p2 - p1))
;;

let get t i = get_opt t i |> or_not_found
let start_opt subs i = offset_opt subs i |> Option.map fst
let start subs i = start_opt subs i |> or_not_found
let stop_opt subs i = offset_opt subs i |> Option.map snd
let stop subs i = stop_opt subs i |> or_not_found
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
