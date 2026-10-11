open Import

type construction =
  | Parse
  | Build

type t =
  { pattern : Re.t
  ; construct : (unit -> Re.t) option
  ; runs : (string * (Re.re -> unit)) list
  ; check : Re.re -> unit
  ; force : [ `Full | `Inputs of string ]
  }

type case =
  { name : string
  ; make : unit -> t
  ; modes : string list
  ; construction : construction option
  ; runtest : bool
  }

let repeat n text = String.concat "" (List.init n (fun _ -> text))
let yes input = input, true
let no input = input, false

let input_runs =
  [ ("execp", fun re input -> ignore (Re.execp re input : bool))
  ; ("exec_opt", fun re input -> ignore (Re.exec_opt re input))
  ; ( "exec"
    , fun re input ->
        match Re.exec re input with
        | _ -> ()
        | exception Not_found -> () )
  ]
;;

let prepare_inputs ?construct ?(check = fun _ -> ()) ~force name pattern samples =
  let check re =
    List.iter
      (fun (input, expected) ->
         let via_exec =
           match Re.exec re input with
           | _ -> true
           | exception Not_found -> false
         in
         if
           via_exec <> expected
           || Re.execp re input <> expected
           || Option.is_some (Re.exec_opt re input) <> expected
         then failwith (Printf.sprintf "%s: unexpected match result for %S" name input))
      samples;
    check re
  in
  let strings = List.map fst samples in
  let runs =
    List.map (fun (mode, run) -> mode, fun re -> List.iter (run re) strings) input_runs
  in
  { pattern; construct; runs; check; force }
;;

(* Metadata is available without constructing patterns or inputs. Each
   constructor supplies both the metadata and the corresponding implementation. *)
let inputs ?(force = `Full) ?check ?(runtest = true) name make =
  { name
  ; modes = List.map fst input_runs
  ; construction = None
  ; runtest
  ; make =
      (fun () ->
        let pattern, samples = make () in
        prepare_inputs ?check ~force name pattern samples)
  }
;;

let perl ?(no_group = false) ?(force = `Full) name make =
  { name
  ; modes = List.map fst input_runs
  ; construction = Some Parse
  ; runtest = true
  ; make =
      (fun () ->
        let source, samples = make () in
        let parse () =
          let pattern = Re.Perl.re source in
          if no_group then Re.no_group pattern else pattern
        in
        prepare_inputs ~construct:parse ~force name (parse ()) samples)
  }
;;

let generated ?(runtest = true) name ~pattern ~samples =
  { name
  ; modes = List.map fst input_runs
  ; construction = Some Build
  ; runtest
  ; make =
      (fun () ->
        prepare_inputs ~construct:pattern ~force:`Full name (pattern ()) (samples ()))
  }
;;

let custom ?(force = `Full) name make =
  { name
  ; modes = [ "run" ]
  ; construction = None
  ; runtest = true
  ; make =
      (fun () ->
        let pattern, run, check = make () in
        { pattern; construct = None; runs = [ "run", run ]; check; force })
  }
;;

let words re = Obj.reachable_words (Obj.repr re)

let print_stats ?compiled_words name re =
  let { Re.Stats.colors; states } = Re.stats re in
  Printf.printf "%s:\n  colors: %s\n  states: %s\n" name (commas colors) (commas states);
  Option.iter
    (fun compiled_words ->
       Printf.printf
         "  compiled_words: %s\n  forced_words: %s\n"
         (commas compiled_words)
         (commas (words re)))
    compiled_words
;;

(* Never force or validate a regex that will be used for timing. Some stress
   cases have exponential complete automata; label their input-forced sizes
   explicitly instead of presenting them as complete automata. *)
let report name t =
  let re = Re.compile t.pattern in
  let compiled_words = words re in
  (match t.force with
   | `Full -> Re.force_states re
   | `Inputs _ -> List.iter (fun (_, run) -> run re) t.runs);
  print_stats ~compiled_words name re;
  (match t.force with
   | `Full -> Printf.printf "  forcing: full\n%!"
   | `Inputs reason -> Printf.printf "  forcing: inputs (%s)\n%!" reason);
  t.check re
;;

let select ~name cases =
  let prefixes =
    Option.map (String.split_on_char ',') (Sys.getenv_opt "RE_BENCH_FILTER")
  in
  let selected =
    List.filter
      (fun case ->
         let name = name case in
         match prefixes with
         | None -> true
         | Some prefixes ->
           List.exists
             (fun prefix ->
                String.length name >= String.length prefix
                && String.sub name 0 (String.length prefix) = prefix)
             prefixes)
      cases
  in
  if selected = [] then failwith "RE_BENCH_FILTER matched no workloads";
  selected
;;
