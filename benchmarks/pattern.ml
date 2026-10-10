open Base
open Import

type case =
  { name : string
  ; pattern : Re.t
  ; yes : string list
  ; no : string list
  ; forceable : bool
  }

let perl ?(forceable = true) name pattern yes no =
  { name; pattern = Re.Perl.re pattern; yes; no; forceable }
;;

let check_samples cases =
  let failures =
    List.concat_map cases ~f:(fun { name; pattern; yes; no; forceable = _ } ->
      let re = Re.compile pattern in
      let yes_failures =
        List.filter_map yes ~f:(fun input ->
          if Re.execp re input then None else Some (Printf.sprintf "%s: %S" name input))
      in
      let no_failures =
        List.filter_map no ~f:(fun input ->
          if Re.execp re input then Some (Printf.sprintf "%s: %S" name input) else None)
      in
      List.map yes_failures ~f:(fun failure -> "missing match: " ^ failure)
      @ List.map no_failures ~f:(fun failure -> "unexpected match: " ^ failure))
  in
  if not (List.is_empty failures) then failwith (String.concat ~sep:"\n" failures)
;;
