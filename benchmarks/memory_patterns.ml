open Import
(* This set of patterns is designed for testing re's memory usage rather than
   speed. *)

let size = 1_000

(* a pathological re that will consume a bunch of memory *)
let re () =
  let open Re in
  seq [ rep (set "01"); char '1'; repn (set "01") size (Some size) ]
;;

(* Another pathological case that is a simplified version of the above *)
let re2 () =
  let open Re in
  seq [ rep (set "01"); char '1'; repn (set "01") size (Some size); char 'x' ]
;;

let str = "01" ^ String.make size '1'
