open Core
open Re_benchmarks
open Import
module Bench = Core_bench.Bench

let benchmarks =
  [ "memory 1", Memory_patterns.re; "memory 2", Memory_patterns.re2 ]
  |> List.map ~f:(fun (name, re) ->
    Bench.Test.create_indexed
      ~name
      ~args:[ 10; 20; 40; 80; 100; Memory_patterns.size ]
      (fun len ->
         Staged.stage (fun () ->
           let re = Re.compile (re ()) in
           let len = Int.min (String.length Memory_patterns.str) len in
           ignore (Re.execp ~pos:0 ~len re Memory_patterns.str))))
;;
