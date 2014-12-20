open Uucd

module Cpset = Set.Make (struct type t = int let compare n m = n - m end)

let collect p r =
  Cpmap.fold (fun cp ps m -> match find ps p with None -> m | Some x -> Cpmap.add cp x m) r Cpmap.empty

let ucd_or_die inf =
  try
    let ic = if inf = "-" then stdin else open_in inf in
    let d = Uucd.decoder (`Channel ic) in
    Printf.eprintf "Loading UCD data, please wait... %!";
    match Uucd.decode d with
    | `Ok db ->
      Printf.eprintf "done.\n%!";
      db
    | `Error e ->
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      Printf.eprintf "Error: %s:%d.%d-%d.%d: %s\n%!" inf l0 c0 l1 c1 e;
      exit 1
  with
  | Sys_error e -> Printf.eprintf "Error: %s\n%!" e; exit 1

let select v p r =
  let s =
    Cpmap.fold (fun cp ps s ->
        match find ps p with None -> s | Some x -> if x = v then Cpset.add cp s else s) r Cpset.empty
  in
  Cpset.fold Cset.add s []

let alphabetic ucd =
  select true alphabetic ucd
let uppercase ucd =
  select true uppercase ucd
let lowercase ucd =
  select true lowercase ucd
let decimal_number ucd =
  select `Nd general_category ucd
let hex_digit ucd =
  select true hex_digit ucd
let control r =
  select `Cc general_category r
let space_separator r =
  select `Zs general_category r
let surrogate r =
  select `Cs general_category r
let unassigned r =
  select `Cn general_category r
let punctuation r =
  let s =
    Cpmap.fold (fun cp ps s ->
        match find ps general_category with
        | None -> s
        | Some (`Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po) -> Cpset.add cp s
        | Some _ -> s) r Cpset.empty
  in
  Cpset.fold Cset.add s []
let white_space ucd =
  select true white_space ucd

let mark r =
  let s =
    Cpmap.fold (fun cp ps s ->
        match find ps general_category with
        | None -> s
        | Some (`Mn | `Mc | `Me) -> Cpset.add cp s
        | Some _ -> s) r Cpset.empty
  in
  Cpset.fold Cset.add s []

let connector_punctuation r =
  select `Pc general_category r

let join_control r =
  select true join_control r

let out ppf name cs =
  Format.fprintf ppf "@[<hov 2>let %s =@ [@ " name;
  List.iter (fun (a, b) -> Format.fprintf ppf "(%i, %i);@ " a b) cs;
  Format.fprintf ppf "]@."

let ucd = "http://www.unicode.org/Public/7.0.0/ucdxml/ucd.all.grouped.zip"
let in_name = Filename.chop_extension (Filename.basename ucd) ^ ".xml"

let download_ucd () =
  let command fmt = Printf.ksprintf Sys.command fmt in
  Printf.eprintf "Downloading %s, please wait...\n%!" ucd;
  let ret = command "curl -O %s" ucd in
  if ret <> 0 then (Printf.eprintf "Error: %d.\n%!" ret; exit 1);
  Printf.eprintf "Unzipping %s, please wait...\n%!" ucd;
  let ret = command "unzip %s" (Filename.basename ucd) in
  if ret <> 0 then (Printf.eprintf "Error: %d.\n%!" ret; exit 1)

let main () =
  let out_name = ref "lib/unicode_groups.ml" in
  let () =
    Arg.parse [] (fun s -> out_name := s) "gen_unicode_tables [output.ml]"
  in
  if not (Sys.file_exists in_name) then download_ucd ();
  let ucd = ucd_or_die in_name in
  let r = ucd.repertoire in
  let oc = open_out !out_name in
  let ppf = Format.formatter_of_out_channel oc in
  Format.fprintf ppf "@[<v 0>";
  out ppf "alphabetic" (alphabetic r);
  out ppf "punctuation" (punctuation r);
  out ppf "uppercase" (uppercase r);
  out ppf "lowercase" (lowercase r);
  out ppf "decimal_number" (decimal_number r);
  out ppf "hex_digit" (hex_digit r);
  out ppf "control" (control r);
  out ppf "white_space" (white_space r);
  out ppf "space_separator" (space_separator r);
  out ppf "unassigned" (unassigned r);
  out ppf "surrogate" (surrogate r);
  out ppf "mark" (mark r);
  out ppf "connector_punctuation" (connector_punctuation r);
  out ppf "join_control" (join_control r);
  Format.fprintf ppf "@]@.";
  close_out oc

let _ =
  main ()