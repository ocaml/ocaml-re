open Core
open Core_bench
open Re_benchmarks
open Import

let test ~name re f =
  [ Bench.Test.create ~name:(sprintf "%s (comp)" name) (fun () -> re ())
  ; Bench.Test.create ~name:(sprintf "%s (comp+exec)" name) (fun () -> f re)
  ; Bench.Test.create_with_initialization ~name:(sprintf "%s (exec)" name) (fun `init ->
      (* No way in core_bench to bench just the exec, so the closest thing we can do is
         bench exec + copy of all the mutable state in a regex (which should be cheap). *)
      let re =
        let re = re () in
        fun () -> Re.copy_re re
      in
      fun () -> f re)
  ]
;;

let exec_bench exec name (re : Re.t) cases =
  Bench.Test.create_group
    ~name
    (List.concat_map cases ~f:(fun data ->
       let name =
         let len = String.length data in
         if len > 70
         then Printf.sprintf "%s .. (%d)" (String.sub data ~pos:0 ~len:10) len
         else data
       in
       let re () = Re.compile re in
       test ~name re (fun re -> ignore (exec (re ()) data))))
;;

let exec_bench_many exec name re cases =
  test
    ~name
    (fun () -> Re.compile re)
    (fun re ->
       let re = re () in
       List.iter cases ~f:(fun x -> ignore (exec re x)))
;;

let string_traversal =
  test
    ~name:"string traversal from #210"
    (fun () -> Re.compile Cases.string_traversal_pattern)
    (fun re -> ignore (Re.execp (re ()) Cases.string_traversal_input ~pos:0))
;;

let compile_clean_star =
  test
    ~name:"kleene star compilation"
    (fun () -> Re.compile Cases.kleene_star_pattern)
    (fun re -> ignore (Re.execp (re ()) Cases.kleene_star_input))
;;

let repeated_sequence =
  test
    ~name:"repeated sequence re"
    (fun () -> Re.compile Cases.repeated_sequence_pattern)
    (fun re -> ignore (Re.execp (re ()) Cases.repeated_sequence_input))
;;

let split =
  test
    ~name:"split on whitespace"
    (fun () -> Re.compile Cases.split_pattern)
    (fun re -> ignore (Re.split_full (re ()) Cases.split_input))
;;

let prefixes =
  let inputs = Array.of_list Cases.prefix_inputs in
  test
    ~name:"shared prefixes"
    (fun () -> Re.compile Cases.prefixes_pattern)
    (fun re ->
       let re = re () in
       Array.iter inputs ~f:(fun str -> ignore (Re.execp re str)))
;;

let duplicate_accepting_states =
  let re = Cases.duplicate_pattern in
  let cases = Cases.duplicate_inputs in
  let bench exec name cases =
    let run re = List.iter cases ~f:(fun input -> ignore (exec re input)) in
    exec_bench_many exec name re cases
    @ [ Bench.Test.create_with_initialization
          ~name:(sprintf "%s (warm exec)" name)
          (fun `init ->
             let re = Re.compile re in
             run re;
             fun () -> run re)
      ]
  in
  Bench.Test.create_group
    ~name:"duplicate accepting states"
    (List.map
       [ "one color", List.take cases 1; "all word colors", cases ]
       ~f:(fun (name, cases) ->
         Bench.Test.create_group
           ~name
           (bench Re.exec "exec" cases @ bench Re.execp "execp" cases)))
;;

let benchmarks =
  let benches =
    List.map Cases.benchmarks ~f:(fun (name, re, cases) ->
      Bench.Test.create_group
        ~name
        [ exec_bench Re.exec "exec" re cases
        ; exec_bench Re.execp "execp" re cases
        ; exec_bench Re.exec_opt "exec_opt" re cases
        ])
  in
  let http_benches =
    let open Http.Export in
    let manual =
      [ request, "no group"; request_g, "group" ]
      |> List.concat_map ~f:(fun (re, name) ->
        let re () = Re.compile re in
        test ~name re (fun re ->
          let re = re () in
          Http.read_all 0 re Http.requests))
      |> Bench.Test.create_group ~name:"manual"
    in
    let many =
      [ test
          ~name:"execp no group"
          (fun () -> Re.compile requests)
          (fun re -> ignore (Re.execp (re ()) Http.requests))
      ; test
          ~name:"all_gen"
          (fun () -> Re.compile requests_g)
          (fun re -> Http.requests |> Re.all (re ()))
      ]
      |> List.concat
      |> Bench.Test.create_group ~name:"auto"
    in
    Bench.Test.create_group ~name:"http" [ manual; many ]
  in
  benches
  @ [ [ exec_bench_many Re.execp "execp"; exec_bench_many Re.exec_opt "exec_opt" ]
      |> List.concat_map ~f:(fun f -> f Tex.ignore_re Tex.ignore_filesnames)
      |> Bench.Test.create_group ~name:"tex gitignore"
    ]
  @ [ http_benches ]
  @ string_traversal
  @ compile_clean_star
  @ Memory.benchmarks
  @ repeated_sequence
  @ split
  @ prefixes
  @ [ duplicate_accepting_states ]
  @ Id_sets.benchmarks
;;

let () =
  let benchmarks =
    match Sys.getenv "RE_BENCH_FILTER" with
    | None -> benchmarks
    | Some only ->
      let only = String.split ~on:',' only in
      let filtered =
        List.filter benchmarks ~f:(fun bench ->
          let name = Bench.Test.name bench in
          List.exists only ~f:(fun pattern -> String.is_prefix ~prefix:pattern name))
      in
      (match filtered with
       | _ :: _ -> filtered
       | [] ->
         print_endline "No benchmarks to run. Your options are:";
         List.iter benchmarks ~f:(fun bench ->
           let name = Bench.Test.name bench in
           Printf.printf "- %s\n" name);
         exit 1)
  in
  Memtrace.trace_if_requested ();
  Command_unix.run (Bench.make_command benchmarks)
;;
