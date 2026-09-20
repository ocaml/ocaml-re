module Re = Re_private.Re
module Stream = Re.Stream

let%expect_test "feeding a terminal stream state raises on JavaScript" =
  (* Native execution can segfault when it indexes the terminal state's
     nonexistent transition table. This test is built only for JavaScript,
     where the same lookup raises a catchable exception. *)
  let plain () =
    let stream = Stream.create Re.(compile (char 'a')) in
    match Stream.feed stream "ab" ~pos:0 ~len:2 with
    | No_match -> assert false
    | Ok stream -> stream
  in
  let grouped () =
    let stream = Stream.Group.create (Stream.create Re.(compile (char 'a'))) in
    match Stream.Group.feed stream "ab" ~pos:0 ~len:2 with
    | No_match -> assert false
    | Ok stream -> stream
  in
  let feed_result = function
    | Stream.Ok _ -> "Ok"
    | No_match -> "No_match"
  in
  let report name f =
    let result =
      try f () with
      | exn -> Printexc.to_string exn
    in
    Printf.printf "%s: %s\n" name result
  in
  let stream = plain () in
  report "Stream.feed" (fun () -> feed_result (Stream.feed stream "c" ~pos:0 ~len:1));
  let stream = plain () in
  report "Stream.finalize" (fun () ->
    string_of_bool (Stream.finalize stream "c" ~pos:0 ~len:1));
  let stream = grouped () in
  report "Stream.Group.feed" (fun () ->
    feed_result (Stream.Group.feed stream "c" ~pos:0 ~len:1));
  let stream = grouped () in
  report "Stream.Group.finalize" (fun () ->
    feed_result (Stream.Group.finalize stream "c" ~pos:0 ~len:1));
  (* 'a' has already matched. Feeding or finalizing with more input should
     retain that result, not raise. Record the current exceptions. *)
  [%expect
    {|
    Stream.feed: Failure("TypeError: Cannot read properties of undefined (reading '1')")
    Stream.finalize: Failure("TypeError: Cannot read properties of undefined (reading '1')")
    Stream.Group.feed: Failure("TypeError: Cannot read properties of undefined (reading '1')")
    Stream.Group.finalize: Failure("TypeError: Cannot read properties of undefined (reading '1')")
    |}]
;;
