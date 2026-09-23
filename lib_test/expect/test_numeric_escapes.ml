module Re = Re_private.Re

(* The PCRE byte matrix covers lowercase spellings and octal. Exercise the
   uppercase hexadecimal digits separately, without repeating that matrix. *)
let%expect_test "uppercase hexadecimal escapes denote every byte" =
  for byte = 0 to 255 do
    let input = String.make 1 (Char.chr byte) in
    let patterns = [ Printf.sprintf "\\x%02X" byte; Printf.sprintf "\\x{%X}" byte ] in
    List.iter
      (fun pattern ->
         let re = Re.(compile (whole_string (Perl.re pattern))) in
         if not (Re.execp re input)
         then failwith (Printf.sprintf "%s does not match byte %d" pattern byte);
         assert (not (Re.execp re (String.make 1 (Char.chr ((byte + 1) mod 256))))))
      patterns
  done;
  [%expect {| |}]
;;
