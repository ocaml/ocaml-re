open Re_benchmarks
open Import
module Bench = Core_bench.Bench

let bench ~name ~make_regex ~make_input =
  let compile () = Re.compile (make_regex ()) in
  let run re input =
    let groups = Re.exec re input in
    if Re.Group.offset groups 0 <> (0, String.length input)
    then failwith "unexpected match offsets"
  in
  [ Bench.Test.create ~name:(name ^ " (comp)") compile
  ; Bench.Test.create_with_initialization ~name:(name ^ " (comp+exec)") (fun `init ->
      let input = make_input () in
      (* Include AST construction and compilation in every cold lifecycle.
         Do not use benchmark.ml's [test] helper: its exec case copies regexes. *)
      fun () -> run (compile ()) input)
  ; Bench.Test.create_with_initialization ~name:(name ^ " (warm exec)") (fun `init ->
      let input = make_input () in
      let re = compile () in
      run re input;
      fun () -> run re input)
  ]
;;

(* The largest cases are deliberately extreme; narrow/8000000 needs several
   GiB of memory. Initialization is deferred until a test is selected.
   For example, from the repository root:

     RE_BENCH_FILTER='expression IDs/broad/65536/262144 (comp+exec)' \
       dune exec --release benchmarks/benchmark.exe -- -quota 55x +time alloc
     RE_BENCH_FILTER='expression IDs/narrow/8000000 (comp+exec)' \
       dune exec --release benchmarks/benchmark.exe -- -quota 55x +time alloc

   A run-count quota supplies enough samples even when one iteration takes
   several seconds. Neither cold nor warm cases copy regexes or reset caches. *)
let benchmarks =
  List.concat
    (List.map
       (fun (branches, length) ->
          bench
            ~name:(Id_patterns.broad_name branches length)
            ~make_regex:(Id_patterns.broad_then_narrow branches length)
            ~make_input:(fun () -> "az" ^ String.make length '0'))
       Id_patterns.broad_params
     @ List.map
         (fun length ->
            bench
              ~name:(Id_patterns.narrow_name length)
              ~make_regex:(Id_patterns.narrow length)
              ~make_input:(fun () -> String.make length '0'))
         Id_patterns.narrow_params)
;;
