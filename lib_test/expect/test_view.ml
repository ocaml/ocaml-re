open Import

let%expect_test "view" =
  let view = Re.View.view (Re.str "foo") in
  ignore view
;;
