open Core

module Both = struct
  type 'a t =
    { lhs : 'a
    ; rhs : 'a
    }
end

module Value = struct
  type t =
    | Int of int
    | Float of float

  let of_string s =
    try Int (Int.of_string s) with
    | _ -> Float (Float.of_string s)
  ;;

  let float_of_value = function
    | Int i -> Float.of_int i
    | Float f -> f
  ;;

  let int_of_value = function
    | Int i -> i
    | Float f -> Float.iround_exn f
  ;;

  let rel_delta x y =
    let x = float_of_value x in
    let y = float_of_value y in
    if Float.( = ) x 0. && Float.( = ) y 0. then 0. else (y -. x) /. x
  ;;

  let string_of_rel_delta x y =
    let x = float_of_value x in
    let y = float_of_value y in
    if Float.( > ) y (x *. 2.)
    then Printf.sprintf "x%.1f" (y /. x)
    else if Float.( < ) y (x /. 5.)
    then Printf.sprintf "/%.1f" (x /. y)
    else if Float.( = ) x 0. && Float.( = ) y 0.
    then "."
    else (
      let d = (y -. x) /. x in
      if Float.is_nan d
      then Float.to_string d
      else (
        let d = Float.round (100. *. d) /. 100. in
        if Float.( = ) d 0. then "." else Printf.sprintf "%+.0f%%" (d *. 100.)))
  ;;

  let string_of_abs_delta x y =
    let i = Float.iround_exn (float_of_value y -. float_of_value x) in
    if i = 0 then "." else (if i > 0 then "+" else "") ^ Int.to_string_hum i
  ;;
end

type 'a bench =
  { name : string
  ; time_per_run_nanos : 'a
  ; major_words_per_run : 'a
  ; promoted_words_per_run : 'a
  ; minor_words_per_run : 'a
  }

let of_sexp (sexp : Sexp.t) =
  match sexp with
  | Atom _ -> failwith "expected list"
  | List fields ->
    let kv (sexp : Sexp.t) =
      match sexp with
      | List [ Atom k; Atom v ] -> Some (k, v)
      | _ -> None
    in
    let fields = List.filter_map fields ~f:kv in
    let field name =
      List.find_map_exn fields ~f:(fun (k, v) ->
        if String.equal k name then Some v else None)
    in
    let name = field "full_benchmark_name" in
    let time_per_run_nanos = Value.of_string (field "time_per_run_nanos") in
    let major_words_per_run = Value.of_string (field "major_words_per_run") in
    let promoted_words_per_run = Value.of_string (field "promoted_words_per_run") in
    let minor_words_per_run = Value.of_string (field "minor_words_per_run") in
    { name
    ; time_per_run_nanos
    ; major_words_per_run
    ; promoted_words_per_run
    ; minor_words_per_run
    }
;;

let parse_all s =
  match Sexp.of_string s with
  | Atom _ -> failwith "list expected"
  | List benches ->
    List.map benches ~f:of_sexp
    |> String.Map.of_list_with_key_exn ~get_key:(fun v -> v.name)
;;

let merge_one
  { name
  ; time_per_run_nanos
  ; major_words_per_run
  ; promoted_words_per_run
  ; minor_words_per_run
  }
  b
  =
  assert (String.equal name b.name);
  { b with
    time_per_run_nanos = { Both.lhs = time_per_run_nanos; rhs = b.time_per_run_nanos }
  ; major_words_per_run = { Both.lhs = major_words_per_run; rhs = b.major_words_per_run }
  ; promoted_words_per_run =
      { Both.lhs = promoted_words_per_run; rhs = b.promoted_words_per_run }
  ; minor_words_per_run = { Both.lhs = minor_words_per_run; rhs = b.minor_words_per_run }
  }
;;

let merge lhs rhs =
  Map.merge lhs rhs ~f:(fun ~key:_ v ->
    match v with
    | `Left _ -> None
    | `Right _ -> None
    | `Both (lhs, rhs) -> Some (merge_one lhs rhs))
;;

let run ~prev ~next ~sort =
  let report =
    let prev = Stdio.In_channel.read_all prev |> parse_all in
    let next = Stdio.In_channel.read_all next |> parse_all in
    merge prev next
  in
  let records =
    let headers =
      [ "name"
      ; "ns/run"
      ; "delta"
      ; "."
      ; "majorW/run"
      ; "delta"
      ; "."
      ; "promotedW/run"
      ; "delta"
      ; "."
      ; "minorW/run"
      ; "delta"
      ; "."
      ]
    in
    let values =
      Map.data report
      |> List.map
           ~f:
             (fun
               ({ name
                ; time_per_run_nanos
                ; major_words_per_run
                ; promoted_words_per_run
                ; minor_words_per_run
                } :
                 Value.t Both.t bench)
             ->
             let time_delta =
               Value.rel_delta time_per_run_nanos.lhs time_per_run_nanos.rhs
             in
             let make_delta { Both.lhs; rhs } =
               [ Int.to_string_hum (Value.int_of_value lhs)
               ; Value.string_of_rel_delta lhs rhs
               ; Value.string_of_abs_delta lhs rhs
               ]
             in
             ( time_delta
             , name
               :: List.concat
                    [ make_delta time_per_run_nanos
                    ; make_delta major_words_per_run
                    ; make_delta promoted_words_per_run
                    ; make_delta minor_words_per_run
                    ] ))
      |> (if sort
          then List.sort ~compare:(fun (x, _) (y, _) -> Float.compare x y)
          else Fn.id)
      |> List.map ~f:snd
    in
    headers :: values
  in
  let chan = Csv.to_channel Stdio.stdout in
  Csv.output_all chan records
;;

let command =
  let ( let+ ) x f = Command.Let_syntax.Let_syntax.map x ~f in
  let ( and+ ) = Command.Let_syntax.Let_syntax.both in
  let open Command.Param in
  Command.basic
    ~summary:"compare two runs"
    (let+ prev = flag "prev" (required string) ~doc:"sexp file"
     and+ next = flag "next" (required string) ~doc:"sexp file"
     and+ sort = flag "sort" no_arg ~doc:"sort lines in order of relative change" in
     fun () -> run ~prev ~next ~sort)
;;

let () = Command_unix.run command
