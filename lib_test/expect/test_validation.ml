open Import

let any = Re.(compile (rep any))

let%expect_test "bound errors" =
  let () = Printexc.record_backtrace false in
  List.iter
    [ (fun () -> Re.execp any ~pos:4 "foo")
    ; (fun () -> Re.execp any ~pos:1 ~len:3 "foo")
    ]
    ~f:(fun f ->
      match f () with
      | (_ : bool) -> print_endline "returned"
      | exception Invalid_argument msg -> Printf.printf "Invalid_argument %S\n" msg);
  [%expect
    {|
    Invalid_argument "Re.exec: out of bounds"
    Invalid_argument "Re.exec: out of bounds"
    |}]
;;
