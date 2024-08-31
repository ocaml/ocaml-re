open Import

let () = Printexc.record_backtrace false
let any = Re.(compile (rep any))

let%expect_test "bound errors" =
  let (_ : bool) = Re.execp any ~pos:4 "foo" in
  [%expect {| |}];
  let (_ : bool) = Re.execp any ~pos:1 ~len:3 "foo" in
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Invalid_argument "Re.exec: out of bounds") |}]
;;
