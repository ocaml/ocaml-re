open Base
open Import

let ignore_re =
  Stdio.In_channel.read_lines "benchmarks/tex.gitignore"
  |> List.map ~f:(fun s ->
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s)
  |> List.filter_map ~f:(fun s ->
    match String.strip s with
    | "" -> None
    | s -> Some s)
  |> List.map ~f:(fun s -> Re.Glob.glob s)
  |> Re.alt
;;

let samples =
  (* In this pinned corpus only *.log, *.idx and *.[1-9] match. The last
     rule is an unanchored search, so it also finds version components. *)
  Stdio.In_channel.read_lines "benchmarks/files"
  |> List.map ~f:(fun input ->
    let numeric_component =
      String.existsi input ~f:(fun i c ->
        Char.equal c '.'
        && i + 1 < String.length input
        && Char.between input.[i + 1] ~low:'1' ~high:'9')
    in
    ( input
    , String.is_suffix input ~suffix:".log"
      || String.is_suffix input ~suffix:".idx"
      || numeric_component ))
;;
