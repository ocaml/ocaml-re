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

let ignore_filesnames = Stdio.In_channel.read_lines "benchmarks/files"
