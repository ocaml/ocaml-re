open! Import


let%expect_test "Category.from_letter" =
  for i = 0 to 255 do
    let cat = Re.Category.from_letter Cset.CodePage.(of_int i |> to_letter) in
    if Cset.(mem (CodePage.of_int i) cword) then assert (Re.Category.(intersect letter cat))
  done
;;

let%expect_test "newline" =
  let cat = Re.Category.from_letter (Re.char '\n') in
  assert (Re.Category.(intersect cat newline));
  assert (Re.Category.(intersect cat not_letter))
;;
