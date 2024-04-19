(* This set of benchmarks is designed for testing re's memory usage rather than
   speed. *)

module Bench = Core_bench.Bench

let size = 1_000

(* a pathological re that will consume a bunch of memory *)
let re =
  let open Re in
  compile @@ seq [ rep (set "01"); char '1'; repn (set "01") size (Some size) ]
;;

let str = "01" ^ String.make size '1'

let benchmarks =
  Bench.Test.create_indexed
    ~name:"pathological re"
    ~args:[ 10; 20; 40; 80; 100; size ]
    (fun len ->
       Base.Staged.stage (fun () ->
         let len = Base.Int.min (String.length str) len in
         ignore (Re.execp ~pos:0 ~len re str)))
;;

let () = Command_unix.run (Bench.make_command [ benchmarks ])
