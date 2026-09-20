module Re = Re_private.Re

let%expect_test "braced hexadecimal escapes preserve digit order" =
  List.iter
    (fun pattern ->
       let re = Re.(compile (whole_string (Perl.re pattern))) in
       for byte = 0 to 255 do
         if Re.execp re (String.make 1 (Char.chr byte))
         then Printf.printf "%s matches byte 0x%02X\n" pattern byte
       done)
    [ {|\x41|}; {|\x{41}|}; {|\x{4}|} ];
  (* The braced and unbraced forms of 41 both match 'A' (0x41). *)
  [%expect
    {|
    \x41 matches byte 0x41
    \x{41} matches byte 0x41
    \x{4} matches byte 0x04
    |}]
;;

let%expect_test "braced hexadecimal escapes denote every byte" =
  for byte = 0 to 255 do
    List.iter
      (fun pattern ->
         let re = Re.(compile (whole_string (Perl.re pattern))) in
         assert (Re.execp re (String.make 1 (Char.chr byte)));
         assert (not (Re.execp re (String.make 1 (Char.chr ((byte + 1) mod 256))))))
      [ Printf.sprintf "\\x{%x}" byte; Printf.sprintf "\\x{%02X}" byte ]
  done;
  [%expect {| |}]
;;
