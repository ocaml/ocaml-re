open Import

let check ?(pos = 0) pattern subject expected =
  let actual =
    Option.map
      (fun groups -> Array.to_list (Re.Group.all_offset groups))
      (Re.exec_opt ~pos (Re.Pcre.regexp pattern) subject)
  in
  let show = function
    | None -> "no match"
    | Some offsets ->
      let show (start, stop) = Printf.sprintf "(%d,%d)" start stop in
      "[" ^ String.concat "; " (List.map offsets ~f:show) ^ "]"
  in
  if Poly.equal actual expected
  then Printf.printf "%S agrees with PCRE\n" pattern
  else
    Printf.printf
      "%S on %S at %d: Re %s; PCRE %s\n"
      pattern
      subject
      pos
      (show actual)
      (show expected)
;;

let%expect_test "outer repetition overrides the priority of a nullable lazy operand" =
  check "(?:a??)*" "a" (Some [ 0, 0 ]);
  check "(a??)*" "a" (Some [ 0, 0; 0, 0 ]);
  check ~pos:1 {|\G(?:a??)*|} "!a" (Some [ 1, 1 ]);
  (* Greedy inner operands do not have the same problem. The final empty
     iteration itself, including its capture, is supported. *)
  check "(?:a?)*" "a" (Some [ 0, 1 ]);
  check "(a?)*" "a" (Some [ 0, 1; 1, 1 ]);
  [%expect
    {|
    "(?:a??)*" on "a" at 0: Re [(0,1)]; PCRE [(0,0)]
    "(a??)*" on "a" at 0: Re [(0,1); (1,1)]; PCRE [(0,0); (0,0)]
    "\\G(?:a??)*" on "!a" at 1: Re [(1,2)]; PCRE [(1,1)]
    "(?:a?)*" agrees with PCRE
    "(a?)*" agrees with PCRE
    |}]
;;
