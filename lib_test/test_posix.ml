open OUnit

let test_class_space () =
  match Re.Posix.compile_pat {|a[[:space:]]b|} with
  | exception Re.Posix.Not_supported -> ()
  | (_ : Re.re) -> assert false

let suite = "posix" >:::
  [ "regression 213" >:: test_class_space
  ]

let () =
  ignore (run_test_tt_main suite)
