open Import

let all_chars = String.init 256 Char.chr

let%expect_test "match an re that distinguishes every single char" =
  let re =
    let open Re in
    set all_chars |> whole_string |> compile
  in
  for i = 0 to String.length all_chars - 1 do
    assert (Re.execp re (String.make 1 all_chars.[i]))
  done
;;
