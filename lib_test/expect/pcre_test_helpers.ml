open Import

(* Characterization checks retain the desired result in the test code, but
   snapshot the actual disagreements. This lets tests precede their fixes.
   Large byte sweeps count every difference and show a few concrete examples. *)
type t =
  { mutable checks : int
  ; mutable differences : int
  ; max_examples : int
  }

let create ?(max_examples = 6) () = { checks = 0; differences = 0; max_examples }

let check t label ~expected actual =
  t.checks <- t.checks + 1;
  if not (String.equal actual expected)
  then (
    t.differences <- t.differences + 1;
    if t.differences <= t.max_examples
    then Printf.printf "%s: %s; expected %s\n" label actual expected)
;;

let finish t =
  if t.differences > t.max_examples
  then Printf.printf "... %d more differences\n" (t.differences - t.max_examples);
  Printf.printf "%d checks; %d differences\n" t.checks t.differences
;;

let error = function
  | `Parse_error -> "parse error"
  | `Not_supported -> "not supported"
;;

let parse_status pattern =
  match Re.Pcre.re_result pattern with
  | Ok _ -> "compiled"
  | Error e -> error e
;;

let with_re ?(flags = []) ?(whole = false) pattern f =
  match Re.Pcre.re_result ~flags pattern with
  | Error e -> error e
  | Ok re -> f (Re.compile (if whole then Re.whole_string re else re))
;;

let with_match ?flags ?whole pattern subject f =
  with_re ?flags ?whole pattern (fun re ->
    match Re.exec_opt re subject with
    | None -> "no match"
    | Some groups -> f re groups)
;;

let text s = Printf.sprintf "%S" s

let match_text ?flags ?whole pattern subject =
  with_match ?flags ?whole pattern subject (fun _ groups -> text (Re.Group.get groups 0))
;;

let bytes_where f =
  let buffer = Buffer.create 256 in
  for code = 0 to 255 do
    if f code then Buffer.add_char buffer (Char.chr code)
  done;
  Buffer.contents buffer
;;

let byte_set ?flags ?(extra = []) pattern =
  with_re ?flags ~whole:true pattern (fun re ->
    let bytes = bytes_where (fun code -> Re.execp re (String.make 1 (Char.chr code))) in
    let extra =
      List.filter ("" :: extra) ~f:(Re.execp re)
      |> List.map ~f:(fun subject -> "; also matches " ^ text subject)
      |> String.concat ""
    in
    text bytes ^ extra)
;;

let list show xs = "[" ^ String.concat "; " (List.map xs ~f:show) ^ "]"
let array show xs = list show (Array.to_list xs)
let offset (start, stop) = Printf.sprintf "(%d,%d)" start stop

let option show = function
  | None -> "unset"
  | Some value -> show value
;;
