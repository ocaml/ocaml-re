module Re = Re_private.Re

let%expect_test "braced octal escapes sum digits instead of decoding positionally" =
  List.iter
    (fun pattern ->
       match Re.Perl.re_result pattern with
       | Error `Parse_error -> Printf.printf "%s: Parse_error\n" pattern
       | Error `Not_supported -> Printf.printf "%s: Not_supported\n" pattern
       | Ok pattern_ast ->
         let re = Re.(compile (whole_string pattern_ast)) in
         for byte = 0 to 255 do
           if Re.execp re (String.make 1 (Char.chr byte))
           then Printf.printf "%s matches byte 0x%02X\n" pattern byte
         done)
    [ {|\111|}; {|\o{111}|}; {|\o{400}|} ];
  (* Both forms of 111 should match 'I' (0x49). Octal 400 is outside the
     byte range and should be rejected. These expectations record the bugs. *)
  [%expect
    {|
    \111 matches byte 0x49
    \o{111} matches byte 0x03
    \o{400} matches byte 0x04
    |}]
;;
