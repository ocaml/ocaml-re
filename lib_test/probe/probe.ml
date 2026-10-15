(* Temporary exploration: do the frontends and executor survive arbitrary bytes? *)

let known_exception = function
  | Re.Perl.Parse_error
  | Re.Posix.Parse_error
  | Re.Pcre.Parse_error
  | Re.Emacs.Parse_error
  | Re.Glob.Parse_error
  | Re.Perl.Not_supported
  | Re.Posix.Not_supported
  | Re.Pcre.Not_supported
  | Re.Emacs.Not_supported -> true
  | _ -> false
;;

let compile_parsers =
  [ (fun s -> Re.Perl.compile_pat s)
  ; (fun s -> Re.Posix.compile_pat s)
  ; (fun s -> Re.Pcre.regexp s)
  ; (fun s -> Re.Emacs.compile_pat s)
  ; (fun s -> Re.compile (Re.Glob.glob s))
  ]
;;

let parse_all pattern =
  List.iter
    (fun compile ->
       match compile pattern with
       | _ -> ()
       | exception exn when known_exception exn -> ())
    compile_parsers
;;

let exec_all pattern input =
  List.iter
    (fun compile ->
       match compile pattern with
       | exception exn when known_exception exn -> ()
       | re ->
         ignore (Re.exec_opt re input);
         ignore (Re.execp re input);
         ignore (Re.exec_partial_detailed re input);
         ignore (Re.all re input);
         ignore (Re.split re input);
         ignore (Re.replace_string re ~by:"x" input))
    compile_parsers
;;

let metachars =
  [| '\\'
   ; '('
   ; ')'
   ; '['
   ; ']'
   ; '{'
   ; '}'
   ; '|'
   ; '*'
   ; '+'
   ; '?'
   ; '.'
   ; '^'
   ; '$'
   ; '-'
   ; ':'
   ; 'a'
   ; 'b'
   ; '0'
   ; '1'
   ; ' '
   ; '\n'
   ; '\255'
  |]
;;

let metachar =
  Crowbar.map [ Crowbar.range (Array.length metachars) ] (fun i -> metachars.(i))
;;

let () =
  Crowbar.add_test ~name:"parse_arbitrary" [ Crowbar.bytes ] parse_all;
  Crowbar.add_test ~name:"parse_then_exec" [ Crowbar.bytes; Crowbar.bytes ] exec_all;
  Crowbar.add_test
    ~name:"parse_metachars"
    Crowbar.[ list metachar ]
    (fun chars ->
       let pattern = String.of_seq (List.to_seq chars) in
       parse_all pattern;
       exec_all pattern "a)\\1}[] ")
;;
