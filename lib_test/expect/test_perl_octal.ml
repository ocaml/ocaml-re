module Re = Re_private.Re

let%expect_test "braced octal escapes decode positionally and stay within byte range" =
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
  (* Both forms of 111 match 'I' (0x49). Octal 400 is outside the byte range. *)
  [%expect
    {|
    \111 matches byte 0x49
    \o{111} matches byte 0x49
    \o{400}: Parse_error
    |}]
;;

let%expect_test "braced octal escapes denote every byte and reject invalid values" =
  for byte = 0 to 255 do
    List.iter
      (fun pattern ->
         let re = Re.(compile (whole_string (Perl.re pattern))) in
         assert (Re.execp re (String.make 1 (Char.chr byte)));
         assert (not (Re.execp re (String.make 1 (Char.chr ((byte + 1) mod 256))))))
      [ Printf.sprintf "\\o{%o}" byte; Printf.sprintf "\\o{000%o}" byte ]
  done;
  List.iter
    (fun pattern ->
       match Re.Perl.re_result pattern with
       | Error `Parse_error -> ()
       | _ -> failwith ("expected Parse_error: " ^ pattern))
    [ {|\o{}|}; {|\o{400}|}; {|\o{777}|}; "\\o{1" ^ String.make 100 '0' ^ "}" ];
  [%expect {| |}]
;;
