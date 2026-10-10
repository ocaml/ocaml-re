open Re_benchmarks
open Import
module Bench = Core_bench.Bench

let benchmarks ({ Workload.name; make; modes; construction; runtest = _ } : Workload.case)
  =
  let construction_phases =
    match construction with
    | None -> []
    | Some Workload.Parse -> [ "parse" ]
    | Some Workload.Build -> [ "build" ]
  in
  (* Filter phase names before constructing workloads, including the multi-GiB
     expression-ID cases. *)
  let workload =
    lazy
      (let t = make () in
       Workload.report name t;
       t)
  in
  let test phase prepare =
    Bench.Test.create_with_initialization
      ~name:(name ^ "/" ^ phase)
      (fun `init -> prepare (Lazy.force workload))
  in
  [ test "compile" (fun t ->
      fun () -> ignore (Sys.opaque_identity (Re.compile t.pattern)))
  ]
  @ (match construction with
     | None -> []
     | Some Parse ->
       [ test "parse" (fun t ->
           let construct = Option.get t.construct in
           fun () -> ignore (Sys.opaque_identity (construct ())))
       ]
     | Some Build ->
       [ test "build+compile" (fun t ->
           let construct = Option.get t.construct in
           fun () -> ignore (Sys.opaque_identity (Re.compile (construct ()))))
       ])
  @ List.concat_map
      (fun mode ->
         let test_run phase prepare =
           test
             (mode ^ "/" ^ phase)
             (fun t -> prepare t (List.assoc mode t.Workload.runs))
         in
         [ test_run "compile+exec" (fun t run -> fun () -> run (Re.compile t.pattern))
         ; test_run "cold" (fun t run ->
             let template = Re.compile t.pattern in
             fun () -> run (Re.copy_re template))
         ; test_run "warm" (fun t run ->
             let re = Re.compile t.pattern in
             run re;
             fun () -> run re)
         ]
         @ List.map
             (fun phase ->
                test_run (phase ^ "+compile+exec") (fun t run ->
                  let construct = Option.get t.construct in
                  fun () -> run (Re.compile (construct ()))))
             construction_phases)
      modes
;;

let () =
  Memtrace.trace_if_requested ();
  let tests =
    List.concat_map benchmarks Suite.cases |> Workload.select ~name:Bench.Test.name
  in
  Command_unix.run (Bench.make_command tests)
;;
