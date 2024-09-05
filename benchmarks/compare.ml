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

  let rec percent_delta x y =
    match x, y with
    | Int x, Int y ->
      let delta = y - x in
      let open Float in
      Float (100. * Float.of_int delta / Float.of_int x)
    | Float x, Float y -> Float Float.(100. * (y - x) / x)
    | Float x, Int y -> percent_delta (Float x) (Float (Float.of_int y))
    | Int x, Float y -> percent_delta (Float (Float.of_int x)) (Float y)
  ;;

  let to_csv t =
    match t with
    | Float f -> Float.to_string_hum f
    | Int x -> Int.to_string_hum x
  ;;

  let compare x y =
    match x, y with
    | Float x, Float y -> Float.compare x y
    | Int x, Int y -> Int.compare x y
    | _, _ -> assert false
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

let run ~prev ~next =
  let report =
    let prev = Stdio.In_channel.read_all prev |> parse_all in
    let next = Stdio.In_channel.read_all next |> parse_all in
    merge prev next
  in
  let records =
    let headers =
      [ "name"
      ; "time_per_run_nanos"
      ; "delta (%)"
      ; "major_words_per_run"
      ; "delta (%)"
      ; "promoted_words_per_run"
      ; "delta (%)"
      ; "minor_words_per_run"
      ; "delta (%)"
      ]
    in
    let values =
      Map.to_alist report
      |> List.map ~f:snd
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
               Value.percent_delta time_per_run_nanos.lhs time_per_run_nanos.rhs
             in
             let make_delta { Both.lhs; rhs } =
               let delta = Value.percent_delta lhs rhs in
               [ Value.to_csv lhs; Value.to_csv delta ]
             in
             ( time_delta
             , name
               :: List.concat
                    [ make_delta time_per_run_nanos
                    ; make_delta major_words_per_run
                    ; make_delta promoted_words_per_run
                    ; make_delta minor_words_per_run
                    ] ))
      |> List.sort ~compare:(fun (x, _) (y, _) -> Value.compare x y)
      |> List.map ~f:snd
    in
    headers :: values
  in
  let chan = Csv.to_channel Stdio.stdout in
  Csv.output_all chan records
;;

let command =
  let open Command.Param in
  let open Command.Param.Applicative_infix in
  Command.basic
    ~summary:"compare two runs"
    (let prev = flag "prev" (required string) ~doc:"sexp file" in
     let next = flag "next" (required string) ~doc:"sexp file" in
     Command.Param.return (fun prev next () -> run ~prev ~next) <*> prev <*> next)
;;

let () = Command_unix.run command
