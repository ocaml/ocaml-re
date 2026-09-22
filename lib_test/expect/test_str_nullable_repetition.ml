module Re = Re_private.Re

(* Snapshot current behavior, including discrepancies with Str. Empty iterations
   should neither beat consuming alternatives nor overwrite earlier captures. *)
module Snapshot (S : module type of Str) = struct
  let run ~partial pattern text groups =
    (* Start from a known match state so partial matching cannot accidentally
       reuse the captures of a preceding full match. *)
    ignore (S.string_match (S.regexp "") "" 0);
    let re = S.regexp pattern in
    let matched =
      if partial then S.string_partial_match re text 0 else S.string_match re text 0
    in
    let captures =
      if not matched
      then []
      else
        List.init (groups + 1) (fun i ->
          match S.matched_group i text with
          | s -> Printf.sprintf "%S" s
          | exception Not_found -> "None"
          | exception exn -> Printexc.to_string exn)
    in
    Printf.sprintf "%b [%s]" matched (String.concat "; " captures)
  ;;
end

module Reference = Snapshot (Str)
module Actual = Snapshot (Re.Str)

let%expect_test "nullable repetitions preserve Str priorities and captures" =
  List.iter
    (fun (pattern, text, groups, full, partial) ->
       List.iter
         (fun (partial, expected) ->
            (* js_of_ocaml's Str loops on some nullable repetitions. Check the
             literal reference on native OCaml and snapshot Re.Str everywhere. *)
            (match Sys.backend_type with
             | Other _ -> ()
             | Native | Bytecode ->
               assert (Reference.run ~partial pattern text groups = expected));
            Printf.printf
              "%s %S %S\n  Str: %s\n  Re.Str: %s\n"
              (if partial then "partial" else "full")
              pattern
              text
              expected
              (Actual.run ~partial pattern text groups))
         [ false, full; true, partial ])
    [ {|\(\(a\)?\)*|}, "b", 2, {|true [""; None; None]|}, {|true [""; None; None]|}
    ; {|\(a*\)*|}, "aa", 1, {|true ["aa"; "aa"]|}, {|true ["aa"; None]|}
    ; ( {|\(\(ab\)?\|a\)+|}
      , "aaa"
      , 2
      , {|true ["aaa"; "a"; None]|}
      , {|true ["aaa"; ""; None]|} )
    ; {|\(a\|aa\)*|}, "aa", 1, {|true ["aa"; "a"]|}, {|true ["aa"; ""]|}
    ; {|\(a?\)\(a?\)*|}, "a", 2, {|true ["a"; "a"; None]|}, {|true ["a"; "a"; None]|}
    ];
  [%expect
    {|
    full "\\(\\(a\\)?\\)*" "b"
      Str: true [""; None; None]
      Re.Str: true [""; ""; None]
    partial "\\(\\(a\\)?\\)*" "b"
      Str: true [""; None; None]
      Re.Str: true [""; ""; None]
    full "\\(a*\\)*" "aa"
      Str: true ["aa"; "aa"]
      Re.Str: true ["aa"; ""]
    partial "\\(a*\\)*" "aa"
      Str: true ["aa"; None]
      Re.Str: true ["aa"; None]
    full "\\(\\(ab\\)?\\|a\\)+" "aaa"
      Str: true ["aaa"; "a"; None]
      Re.Str: true ["aaa"; ""; None]
    partial "\\(\\(ab\\)?\\|a\\)+" "aaa"
      Str: true ["aaa"; ""; None]
      Re.Str: true ["aaa"; ""; None]
    full "\\(a\\|aa\\)*" "aa"
      Str: true ["aa"; "a"]
      Re.Str: true ["aa"; "a"]
    partial "\\(a\\|aa\\)*" "aa"
      Str: true ["aa"; ""]
      Re.Str: true ["aa"; ""]
    full "\\(a?\\)\\(a?\\)*" "a"
      Str: true ["a"; "a"; None]
      Re.Str: true ["a"; "a"; ""]
    partial "\\(a?\\)\\(a?\\)*" "a"
      Str: true ["a"; "a"; None]
      Re.Str: true ["a"; "a"; None]
    |}]
;;
