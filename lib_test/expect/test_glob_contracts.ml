module Re = Re_private.Re

let%expect_test "glob ranges, escapes, lookahead and brace expansion" =
  let check ?(pathname = true) ?(period = true) ?(expand_braces = false) pattern yes no =
    let r =
      Re.Glob.glob ~anchored:true ~pathname ~period ~expand_braces pattern |> Re.compile
    in
    List.iter
      (fun s -> if not (Re.execp r s) then failwith (pattern ^ " should match " ^ s))
      yes;
    List.iter
      (fun s -> if Re.execp r s then failwith (pattern ^ " should not match " ^ s))
      no
  in
  check "[a-c]" [ "a"; "b"; "c" ] [ ""; "d"; "ab" ];
  check "[c-a]" [ "a"; "b"; "c" ] [ "d" ];
  check "[a-]" [ "a"; "-" ] [ "b" ];
  check "*?" [ "a"; "ab" ] [ ""; ".a"; "/a" ];
  check "*[!a]" [ "b"; "ab"; "a." ] [ "a"; ".b"; "a/" ];
  check ~pathname:false "*" [ ""; "a/b"; "a/.b"; "a.b" ] [ ".a" ];
  check ~pathname:false ~period:false "*" [ ".a"; "a/.b" ] [];
  check ~period:false "*?" [ ".a"; "." ] [ ""; "a/b" ];
  check ~expand_braces:true "a{b,{c,d}}{1,2}" [ "ab1"; "ac2"; "ad1" ] [ "a1"; "ae1" ];
  check ~expand_braces:true {|{a\,b,c}|} [ "a,b"; "c" ] [ "a"; "b" ];
  check ~expand_braces:true {|\{a,b\}|} [ "{a,b}" ] [ "a"; "b" ];
  List.iter
    (fun (expand_braces, pattern) ->
       assert (Re.Glob.glob_result ~expand_braces pattern = Error `Parse_error))
    [ false, "["
    ; false, "[a"
    ; false, "[a-"
    ; false, "\\"
    ; true, "a{b,c"
    ; true, "{a,{b,c}"
    ];
  List.iter
    (fun pattern ->
       match
         Re.Glob.glob_result
           ~anchored:true
           ~period:false
           ~pathname:false
           ~match_backslashes:true
           ~expand_braces:true
           ~double_asterisk:false
           pattern
       with
       | Error _ -> assert false
       | Ok r ->
         let expected =
           Re.Glob.glob
             ~anchored:true
             ~period:false
             ~pathname:false
             ~match_backslashes:true
             ~expand_braces:true
             ~double_asterisk:false
             pattern
         in
         List.iter
           (fun s ->
              assert (Re.execp (Re.compile r) s = Re.execp (Re.compile expected) s))
           [ "a"; "a/b"; "a\\b"; ".a"; "" ])
    [ "*"; "a/b"; "{a,b}" ];
  List.iter
    (fun (old, modern) ->
       List.iter
         (fun s -> assert (Re.execp (Re.compile old) s = Re.execp (Re.compile modern) s))
         [ ""; "a"; ".a"; "b"; "xa" ])
    [ ( Re.Glob.glob' ~anchored:true false "*"
      , Re.Glob.glob ~anchored:true ~period:false "*" )
    ; ( Re.Glob.globx ~anchored:true "{a,b}"
      , Re.Glob.glob ~anchored:true ~expand_braces:true "{a,b}" )
    ; ( Re.Glob.globx' ~anchored:true false "{a,*}"
      , Re.Glob.glob ~anchored:true ~period:false ~expand_braces:true "{a,*}" )
    ];
  [%expect {| |}]
;;

let strings alphabet max =
  let rec exact n =
    if n = 0
    then [ "" ]
    else
      List.concat_map
        (fun s -> List.map (fun c -> String.make 1 c ^ s) alphabet)
        (exact (n - 1))
  in
  List.concat (List.init (max + 1) exact)
;;

let%expect_test "all small wildcard patterns agree with a direct matcher" =
  let reference pattern input =
    let rec loop p i =
      if p = String.length pattern
      then i = String.length input
      else (
        match pattern.[p] with
        | '*' -> loop (p + 1) i || (i < String.length input && loop p (i + 1))
        | '?' -> i < String.length input && loop (p + 1) (i + 1)
        | c -> i < String.length input && c = input.[i] && loop (p + 1) (i + 1))
    in
    loop 0 0
  in
  let inputs = strings [ 'a'; 'b' ] 4 in
  List.iter
    (fun pattern ->
       let re =
         Re.Glob.glob ~anchored:true ~double_asterisk:false pattern |> Re.compile
       in
       List.iter
         (fun input ->
            if Re.execp re input <> reference pattern input
            then failwith (Printf.sprintf "glob %S input %S" pattern input))
         inputs)
    (strings [ 'a'; 'b'; '*'; '?' ] 3);
  [%expect {| |}]
;;
