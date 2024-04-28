(* This set of benchmarks is designed for testing re's memory usage rather than
   speed. *)

module Bench = Core_bench.Bench

let size = 1_000

(* a pathological re that will consume a bunch of memory *)
let re =
  let open Re in
  compile @@ seq [ rep (set "01"); char '1'; repn (set "01") size (Some size) ]
;;

(* Another pathological case that is a simplified version of the above *)
let re2 =
  let open Re in
  seq [ rep (set "01"); char '1'; repn (set "01") size (Some size); char 'x' ] |> compile
;;

let str = "01" ^ String.make size '1'

let benchmarks =
  [ "re", re; "re2", re2 ]
  |> ListLabels.map ~f:(fun (name, re) ->
    Bench.Test.create_indexed ~name ~args:[ 10; 20; 40; 80; 100; size ] (fun len ->
      Base.Staged.stage (fun () ->
        let len = Base.Int.min (String.length str) len in
        ignore (Re.execp ~pos:0 ~len re str))))
;;

let () = Command_unix.run (Bench.make_command benchmarks)
