module Category = Re_private.Category
module Cset = Re_private.Cset

let%expect_test "Category.from_char" =
  for i = 0 to 255 do
    let char = Char.chr i in
    let cat = Category.from_char char in
    if Cset.(mem (of_char char) cword) then assert (Category.(intersect letter cat))
  done
;;

let%expect_test "newline" =
  let cat = Category.from_char '\n' in
  assert (Category.(intersect cat newline));
  assert (Category.(intersect cat not_letter))
;;
