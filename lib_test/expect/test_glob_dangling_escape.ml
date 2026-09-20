module Re = Re_private.Re

let%expect_test "brace expansion reads past a dangling escape" =
  List.iter
    (fun expand_braces ->
       List.iter
         (fun pattern ->
            let result =
              try
                let re = Re.(compile (whole_string (Glob.glob ~expand_braces pattern))) in
                Printf.sprintf "match %S" (Re.Group.get (Re.exec re "a*") 0)
              with
              | Re.Glob.Parse_error -> "Re.Glob.Parse_error"
              | exn -> Printexc.to_string exn
            in
            Printf.printf "expand_braces=%b %S: %s\n" expand_braces pattern result)
         [ "a\\*"; "a\\" ])
    [ false; true ];
  (* A trailing backslash is invalid with or without brace expansion and
     should raise Glob.Parse_error, not leak an indexing exception. *)
  [%expect
    {|
    expand_braces=false "a\\*": match "a*"
    expand_braces=false "a\\": Re.Glob.Parse_error
    expand_braces=true "a\\*": match "a*"
    expand_braces=true "a\\": Invalid_argument("String.sub / Bytes.sub")
    |}]
;;
