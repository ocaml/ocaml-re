open Core

let ignore_re =
  Stdio.In_channel.read_lines "benchmarks/tex.gitignore"
  |> List.map ~f:(fun s ->
    match Base.String.lsplit2 s ~on:'#' with
    | Some (pattern, _comment) -> pattern
    | None -> s)
  |> List.filter_map ~f:(fun s ->
    match Base.String.strip s with
    | "" -> None
    | s -> Some s)
  |> List.map ~f:Re.Glob.glob
  |> Re.alt
;;

let ignore_filesnames = Stdio.In_channel.read_lines "benchmarks/files"
