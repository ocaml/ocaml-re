open Import

(* let pp_letters ppf s =
  Format.pp_print_iter
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    Cset.Codec.iter
    (fun fmt letter ->
      Format.fprintf fmt "%a" Cset.CodePage.pp
        (Cset.CodePage.from_letter letter))
    ppf s *)
let pp_letters ppf s =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    (fun fmt letter ->
      Format.fprintf fmt "%a" Cset.CodePage.pp
        (Cset.CodePage.from_letter letter))
    ppf (Cset.Codec.to_list s)

let l = [ (5, 255) ]

let all_chars =
  let bytes = Bytes.create (256 * 4) in
  let pos = ref 0 in
  Stdlib.List.iter
    (fun (i1, i2) ->
      for i = i1 to i2 do
        pos := !pos + Cset.Codec.set bytes !pos (Cset.CodePage.of_char @@ Char.chr i)
      done)
    l;
  Bytes.sub_string bytes 0 !pos

let%expect_test "match an re that distinguishes every single char" =
  (* Format.printf "[Unicode.Test_color] all_chars: '%a'\n" pp_letters all_chars; *)
  let all_chars = Re.set all_chars in
  (* Format.printf "[Unicode.Test_color] set: '%a'\n" Re.pp all_chars; *)
  let whole_string = Re.whole_string all_chars in
  (* Format.printf "[Unicode.Test_color] whole_string: '%a'\n" Re.pp whole_string; *)
  let re = Re.compile whole_string in
  (* Format.printf "[Unicode.Test_color] re: '%a'\n" Re.pp_re re; *)
  Stdlib.List.iter
    (fun (c1, c2) ->
      for i = c1 to c2 do
        assert (Re.execp re (string_make_of_int i))
      done)
    l
