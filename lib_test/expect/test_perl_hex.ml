module Re = Re_private.Re

let%expect_test "braced hexadecimal escapes reverse the digits" =
  List.iter
    (fun pattern ->
       let re = Re.(compile (whole_string (Perl.re pattern))) in
       for byte = 0 to 255 do
         if Re.execp re (String.make 1 (Char.chr byte))
         then Printf.printf "%s matches byte 0x%02X\n" pattern byte
       done)
    [ {|\x41|}; {|\x{41}|}; {|\x{4}|} ];
  (* The braced and unbraced forms of 41 should both match 'A' (0x41).
     Record the current reversed value of the two-digit braced form. *)
  [%expect
    {|
    \x41 matches byte 0x41
    \x{41} matches byte 0x14
    \x{4} matches byte 0x04
    |}]
;;
