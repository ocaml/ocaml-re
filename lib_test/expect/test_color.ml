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

let%expect_test "the set of colors should be as small as easily feasible" =
  let re = Re.(compile wordc) in
  print_dyn (Re_private.Compile.to_dyn ~color_map:true re);
  [%expect
    {|
    ((initial (Seq:S (Rep:GS ((0 1))) (Seq:F (Mark 0) 1 (Mark 1))))
     (color_map
      ((0
        ((0 47) (58 64) (91 94) (96 96) (123 169) (171 180) (182 185) (187 191)
         (215 215) (247 247)))
       (1
        ((48 57) (65 90) (95 95) (97 122) (170 170) (181 181) (186 186) (192 214)
         (216 246) (248 255))))))
    |}]
;;
