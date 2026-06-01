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
    ((initial
      (short
       (Seq (Rep ((0 19)))
        (first
         (Seq (Mark 0)
          (Seq
           ((1 1) (3 3) (5 5) (7 7) (9 9) (11 11) (13 13) (15 15) (17 17)
            (19 19))
           (Mark 1)))))))
     (color_map
      ((0 ((0 47))) (1 ((48 57))) (2 ((58 64))) (3 ((65 90))) (4 ((91 94)))
       (5 95) (6 96) (7 ((97 122))) (8 ((123 169))) (9 170) (10 ((171 180)))
       (11 181) (12 ((182 185))) (13 186) (14 ((187 191))) (15 ((192 214)))
       (16 215) (17 ((216 246))) (18 247) (19 ((248 255))))))
    |}]
;;
