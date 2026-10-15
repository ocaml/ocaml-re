open Import
module Stream = Re.Stream

let ok = function
  | Stream.Ok x -> x
  | No_match -> failwith "unexpected No_match"
;;

let group_stream re = Stream.Group.create (Stream.create re)

let%expect_test "group finalization recognizes the last newline" =
  let re = Re.compile Re.(seq [ str "a"; leol ]) in
  printf "exec: %b\n" (Re.execp re "a\n");
  List.iter
    [ "a\n", 0; "_a\n__", 1 ]
    ~f:(fun (s, pos) ->
      match Stream.Group.finalize (group_stream re) s ~pos ~len:2 with
      | No_match -> print_endline "stream: No_match"
      | Ok m ->
        Format.printf
          "stream: %a@."
          (Fmt.opt Fmt.quoted_string)
          (Stream.Group.Match.get m 0));
  [%expect
    {|
    exec: true
    stream: "a"
    stream: "a"
    |}]
;;

let%expect_test "a fed final newline is retained until finalization" =
  let re = Re.compile Re.(seq [ str "a"; leol ]) in
  printf "exec: %b\n" (Re.execp re "a\n");
  let t = ok (Stream.feed (Stream.create re) "a\n" ~pos:0 ~len:2) in
  printf "stream: %b\n" (Stream.finalize t "" ~pos:0 ~len:0);
  [%expect
    {|
    exec: true
    stream: true
    |}]
;;

let%expect_test "the last newline is relative to the finalized slice" =
  let re = Re.compile Re.(seq [ str "a"; leol ]) in
  List.iter [ "a\n"; "a\nx" ] ~f:(fun s ->
    printf "%S: %b\n" s (Stream.finalize (Stream.create re) s ~pos:0 ~len:2));
  [%expect
    {|
    "a\n": true
    "a\nx": true
    |}]
;;

let%expect_test "negative chunk lengths must be rejected" =
  let t = Stream.create (Re.compile Re.epsilon) in
  invalid_argument (fun () ->
    match Stream.feed t "" ~pos:0 ~len:(-1) with
    | Ok _ -> print_endline "Ok"
    | No_match -> print_endline "No_match");
  [%expect {| Invalid_argument "Re.Stream: out of bounds" |}]
;;

let%expect_test "a saved group match survives reuse of its stream" =
  let t = group_stream (Re.compile Re.(whole_string (rep any))) in
  let m = ok (Stream.Group.finalize t "a" ~pos:0 ~len:1) in
  let print () =
    match Stream.Group.Match.get m 0 with
    | result -> Format.printf "%a@." (Fmt.opt Fmt.quoted_string) result
    | exception Assert_failure _ -> print_endline "Assert_failure"
  in
  print ();
  ignore (ok (Stream.Group.finalize t "abc" ~pos:0 ~len:3));
  print ();
  [%expect
    {|
    "a"
    "a"
    |}]
;;
