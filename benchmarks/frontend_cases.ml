open Workload

let parser =
  List.map
    (fun (name, expected, source) ->
       perl ("parser/" ^ name) (fun () -> source (), [ "", expected ]))
    [ ("empty", true, fun () -> "")
    ; ("one byte", false, fun () -> "a")
    ; ("short literal", false, fun () -> "path/to/file.txt")
    ; ("small alternation", false, fun () -> "a|b|c|d")
    ; ("captures", false, fun () -> repeat 128 "(a)")
    ; ("noncapturing", false, fun () -> repeat 128 "(?:ab)")
    ; ("empty groups", true, fun () -> repeat 128 "()")
    ; ("classes", false, fun () -> repeat 128 "[abc]")
    ; ("ranges", false, fun () -> repeat 128 "[a-zA-Z0-9_]")
    ; ("escapes", false, fun () -> repeat 128 "\\w\\d\\s\\b\\A\\z")
    ; ("numeric escapes", false, fun () -> repeat 128 "\\x41\\x{42}\\o{103}\\104")
    ; ("named groups", false, fun () -> repeat 128 "(?<capture_12>a)")
    ; ("long name", false, fun () -> "(?<" ^ String.make 4096 'n' ^ ">a)")
    ; ("comment 4K", true, fun () -> "(?#" ^ String.make 4096 'x' ^ ")")
    ; ("comment 64K", true, fun () -> "(?#" ^ String.make 65536 'x' ^ ")")
    ; ("quoted backslashes", false, fun () -> "\\Q" ^ repeat 512 "a\\\\b\\q" ^ "\\E")
    ]
  @ [ perl
        ~force:(`Inputs "exponential complete automaton")
        "parser/quantifiers"
        (fun () -> repeat 128 "a*?b+c??d{2,4}?", [ no "" ])
    ]
;;

let literal length =
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_" in
  let state = ref 17 in
  String.init length (fun _ ->
    state := ((!state * 251) + 17) mod 65521;
    alphabet.[!state mod String.length alphabet])
;;

let literals =
  List.concat_map
    (fun length ->
       List.filter_map
         (fun shape ->
            (* Deep factoring of a 64K common prefix is a separate stress test. *)
            if length = 65536 && shape = "alternation"
            then None
            else
              Some
                (perl
                   ~no_group:true
                   (Printf.sprintf "literal/%s/%d" shape length)
                   (fun () ->
                      let text = literal length in
                      let source, strings =
                        match shape with
                        | "plain" -> text, [ text ]
                        | "quoted" -> "\\Q" ^ text ^ "\\E", [ text ]
                        | "mixed" -> "\\A(" ^ text ^ ")+[0-9]?\\z", [ text ]
                        | _ -> text ^ "X|" ^ text ^ "Y", [ text ^ "X"; text ^ "Y" ]
                      in
                      source, List.map yes strings)))
         [ "plain"; "quoted"; "mixed"; "alternation" ])
    [ 32; 256; 4096; 65536 ]
  @ List.concat_map
      (fun length ->
         List.map
           (fun kind ->
              perl ~no_group:true (Printf.sprintf "literal/%s/%d" kind length) (fun () ->
                let text = literal length in
                let padding = String.make 65536 '~' in
                let source, input, matched =
                  match kind with
                  | "hit" -> text, padding ^ text, true
                  | "absent" -> text, padding, false
                  | "late miss" ->
                    text, padding ^ String.sub text 0 (length - 1) ^ "~", false
                  | _ -> "\\A" ^ text ^ "\\z", text, true
                in
                source, [ input, matched ]))
           [ "hit"; "absent"; "late miss"; "anchored" ])
      [ 32; 256; 4096 ]
  @ [ perl ~no_group:true "literal/overlap/256" (fun () ->
        String.make 255 'a' ^ "b", [ no (String.make 8192 'a') ])
    ]
;;

(* github/gitignore, 356fd7baab4c05e092194a41f64dbd5afc8817e4 (CC0-1.0).
   705 Joomla rules and 129 literal TeX suffix rules, in source order.
   These are positive-rule regex workloads, not a gitignore implementation.
   Expected paths were checked with git check-ignore --no-index. *)
let gitignore =
  List.concat_map
    (fun name ->
       List.map
         (fun capturing ->
            let variant = if capturing then "capturing" else "noncapturing" in
            perl
              ~no_group:true
              ("gitignore/" ^ name ^ "/" ^ variant)
              (fun () ->
                 let prefix = "benchmarks/gitignore/" ^ name in
                 let source =
                   Stdio.In_channel.read_lines (prefix ^ ".pcre")
                   |> List.map (fun branch ->
                     if capturing
                     then "(" ^ branch ^ ")"
                     else (
                       let branch =
                         Base.String.substr_replace_all
                           branch
                           ~pattern:"(^|/)"
                           ~with_:"(?:^|/)"
                         |> Base.String.substr_replace_all
                              ~pattern:"(/|\\z)"
                              ~with_:"(?:/|\\z)"
                       in
                       "(?:" ^ branch ^ ")"))
                   |> String.concat "|"
                 in
                 let samples =
                   Stdio.In_channel.read_lines (prefix ^ ".cases")
                   |> List.map (fun line ->
                     match String.split_on_char '\t' line with
                     | [ "1"; path ] -> yes path
                     | [ "0"; path ] -> no path
                     | _ -> failwith ("invalid gitignore case: " ^ line))
                 in
                 source, samples))
         [ true; false ])
    [ "joomla"; "latex-ignore-suffixes" ]
;;

let cases = parser @ literals @ gitignore
