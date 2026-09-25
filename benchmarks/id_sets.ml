module Bench = Core_bench.Bench

let exact_bits length = Re.repn (Re.set "01") length (Some length)
let anchored re = Re.seq [ Re.bos; re; Re.eos ]

let broad_then_narrow branches length () =
  (* Distinct classes all accept 'a', but cannot be factored into one shared
     prefix. After 'a', every branch has its own live continuation ID. Only
     the last branch accepts 'z'; the fixed-length tail then has few live IDs
     but must construct a different state for each position.

     A hash table retains the broad prefix's capacity and clears it at every
     tail transition. Generation stamps refill that storage only on wrap. *)
  let branch i =
    let chars = Buffer.create 17 in
    Buffer.add_char chars 'a';
    for bit = 0 to 15 do
      if i land (1 lsl bit) <> 0 then Buffer.add_char chars (Char.chr bit)
    done;
    Re.seq
      [ Re.set (Buffer.contents chars); Re.char (if i = branches - 1 then 'z' else 'b') ]
  in
  anchored (Re.seq [ Re.alt (List.init branches branch); exact_bits length ])
;;

let narrow length () =
  (* Fixed repetition expands to many distinct expression IDs, but each
     derivative has few live IDs. Stamps retain a large byte table and refill
     it every 255 clears; the hash table stays small. There are no inactive
     branches: the match traverses the entire repetition. *)
  anchored (exact_bits length)
;;

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
            ~name:(Printf.sprintf "expression IDs/broad/%d/%d" branches length)
            ~make_regex:(broad_then_narrow branches length)
            ~make_input:(fun () -> "az" ^ String.make length '0'))
       [ 16, 1024; 4096, 16384; 65536, 262144 ]
     @ List.map
         (fun length ->
            bench
              ~name:(Printf.sprintf "expression IDs/narrow/%d" length)
              ~make_regex:(narrow length)
              ~make_input:(fun () -> String.make length '0'))
         [ 1024; 1000000; 8000000 ])
;;
