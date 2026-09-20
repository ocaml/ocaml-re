open Import

let any = Re.(compile (rep any))

let%expect_test "bound errors" =
  let () = Printexc.record_backtrace false in
  invalid_argument (fun () -> Re.execp any ~pos:4 "foo");
  [%expect {| Invalid_argument "Re.exec: out of bounds" |}];
  invalid_argument (fun () -> Re.execp any ~pos:1 ~len:3 "foo");
  [%expect {| Invalid_argument "Re.exec: out of bounds" |}]
;;
