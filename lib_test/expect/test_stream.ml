open Import
module Stream = Re.Stream

let feed t str =
  let res = Stream.feed t str ~pos:0 ~len:(String.length str) in
  let () =
    match res with
    | No_match -> Printf.printf "%S did not match\n" str
    | Ok s ->
      let status =
        match Stream.finalize s "" ~pos:0 ~len:0 with
        | true -> "matched"
        | false -> "unmatched"
      in
      Printf.printf "%S not matched (status = %s)\n" str status
  in
  res
;;

let%expect_test "out out of bounds" =
  let stream = Re.any |> Re.compile |> Stream.create in
  invalid_argument (fun () -> ignore (Stream.feed stream "foo" ~pos:2 ~len:3));
  [%expect {| Invalid_argument "Re.Stream: out of bounds" |}];
  invalid_argument (fun () -> ignore (Stream.finalize stream "foo" ~pos:2 ~len:3));
  [%expect {| Invalid_argument "Re.Stream: out of bounds" |}];
  let stream = Stream.Group.create stream in
  invalid_argument (fun () -> ignore (Stream.Group.feed stream "foo" ~pos:2 ~len:3));
  [%expect {| Invalid_argument "Re.Stream: out of bounds" |}];
  invalid_argument (fun () -> ignore (Stream.Group.finalize stream "foo" ~pos:2 ~len:3));
  [%expect {| Invalid_argument "Re.Stream: out of bounds" |}]
;;

let%expect_test "basic" =
  let s = [ Re.bos; Re.str "abab" ] |> Re.seq |> Re.compile |> Stream.create in
  ignore (feed s "x");
  [%expect {| "x" did not match |}];
  let suffix = "ab" in
  let s =
    match feed s suffix with
    | Ok s -> s
    | No_match -> assert false
  in
  [%expect
    {|
    "ab" not matched (status = unmatched) |}];
  (let (_ : _ Stream.feed) = feed s "ab" in
   [%expect
     {|
     "ab" not matched (status = matched) |}]);
  let (_ : _ Stream.feed) = feed s "xy" in
  [%expect
    {|
    "xy" did not match |}]
;;

let%expect_test "eos" =
  let s = [ Re.str "zzz"; Re.eos ] |> Re.seq |> Re.compile |> Stream.create in
  ignore (feed s "zzz");
  [%expect {| "zzz" not matched (status = matched) |}];
  let s =
    match feed s "z" with
    | Ok s -> s
    | No_match -> assert false
  in
  [%expect {| "z" not matched (status = unmatched) |}];
  (let str = "zz" in
   match Stream.finalize s str ~pos:0 ~len:(String.length str) with
   | true -> ()
   | false -> assert false);
  [%expect {||}]
;;

let%expect_test "finalize empty" =
  let s = "abde" in
  let stream =
    let stream = Re.str s |> Re.whole_string |> Re.compile |> Stream.create in
    match feed stream s with
    | Ok s -> s
    | No_match -> assert false
  in
  assert (Stream.finalize stream "" ~pos:0 ~len:0);
  [%expect {| "abde" not matched (status = matched) |}]
;;

let%expect_test "group - basic" =
  let s =
    let open Re in
    str "foo" |> whole_string |> group |> compile |> Stream.create
  in
  let g = Stream.Group.create s in
  let g =
    match Stream.Group.feed g "f" ~pos:0 ~len:1 with
    | No_match -> assert false
    | Ok s -> s
  in
  (match Stream.Group.finalize g "oo" ~pos:0 ~len:2 with
   | Ok _ -> ()
   | No_match -> assert false);
  [%expect {| |}]
;;

let pmarks set m =
  Printf.printf "mark present %b\n" (Re.Stream.Group.Match.test_mark set m)
;;

let%expect_test "group - mark entire string must match" =
  let m1, f = Re.(mark (char 'f')) in
  let m2, oo = Re.(mark (str "oo")) in
  let re =
    let open Re in
    [ f; oo ] |> seq |> compile
  in
  let s = Stream.create re in
  let g = Stream.Group.create s in
  let g =
    match Stream.Group.feed g "f" ~pos:0 ~len:1 with
    | No_match -> assert false
    | Ok s -> s
  in
  let g =
    match Stream.Group.finalize g "oo" ~pos:0 ~len:2 with
    | Ok g -> g
    | No_match -> assert false
  in
  pmarks g m1;
  [%expect {| mark present true |}];
  pmarks g m2;
  [%expect {| mark present true |}]
;;

let%expect_test "group - partial mark match" =
  let m, foo = Re.(mark (str "foo")) in
  let re = Re.compile foo in
  let s = Stream.create re in
  let g = Stream.Group.create s in
  let g =
    match Stream.Group.feed g "xx" ~pos:0 ~len:2 with
    | No_match -> assert false
    | Ok g -> g
  in
  let g =
    match Stream.Group.feed g "foo" ~pos:0 ~len:3 with
    | Ok g -> g
    | No_match -> assert false
  in
  let g =
    match Stream.Group.finalize g "garb" ~pos:0 ~len:4 with
    | Ok g -> g
    | No_match -> assert false
  in
  pmarks g m;
  [%expect {| mark present true |}]
;;

let print_match match_ n =
  match Stream.Group.Match.get match_ n with
  | None -> Printf.printf "match %d: <not found>\n" n
  | Some s -> Printf.printf "match %d: %s\n" n s
;;

let%expect_test "group - match group" =
  let stream =
    let re = Re.Pcre.re "_([a-z]+)_" |> Re.whole_string |> Re.compile in
    Stream.Group.create (Stream.create re)
  in
  let s = "_abc_" in
  let () =
    match Stream.Group.finalize stream s ~pos:0 ~len:(String.length s) with
    | No_match -> assert false
    | Ok m ->
      for i = 0 to 1 do
        print_match m i
      done
  in
  [%expect
    {|
    match 0: _abc_
    match 1: abc
    |}]
;;

let%expect_test "group - match group" =
  let stream =
    let re = Re.Pcre.re "_([a-z]+)__([a-z]+)_" |> Re.whole_string |> Re.compile in
    Stream.Group.create (Stream.create re)
  in
  let s = "_abc_" in
  let stream =
    match Stream.Group.feed stream s ~pos:0 ~len:(String.length s) with
    | No_match -> assert false
    | Ok m -> m
  in
  let s = "_de_" in
  let () =
    match Stream.Group.finalize stream s ~pos:0 ~len:(String.length s) with
    | No_match -> assert false
    | Ok m ->
      for i = 0 to 2 do
        print_match m i
      done
  in
  [%expect
    {|
    match 0: _abc__de_
    match 1: abc
    match 2: de
    |}]
;;

let%expect_test "group - captures with nonzero source offsets" =
  let run ~feed_pos ~finalize_pos =
    let re = Re.(compile (whole_string (seq [ char 'a'; group (char 'b'); char 'c' ]))) in
    let stream = Stream.Group.create (Stream.create re) in
    let feed stream s ~pos =
      match Stream.Group.feed stream s ~pos ~len:1 with
      | No_match -> assert false
      | Ok stream -> stream
    in
    let stream = feed stream "a" ~pos:0 in
    let stream = feed stream (String.make feed_pos 'x' ^ "b") ~pos:feed_pos in
    match
      Stream.Group.finalize
        stream
        (String.make finalize_pos 'x' ^ "c")
        ~pos:finalize_pos
        ~len:1
    with
    | No_match -> assert false
    | Ok matched ->
      let whole = Option.get (Stream.Group.Match.get matched 0) in
      let capture = Option.get (Stream.Group.Match.get matched 1) in
      Printf.printf
        "feed pos=%d, finalize pos=%d: whole=%S; group 1=%S\n"
        feed_pos
        finalize_pos
        whole
        capture
  in
  (* All calls stream "abc", so group 0 is "abc" and group 1 is "b". *)
  run ~feed_pos:0 ~finalize_pos:0;
  run ~feed_pos:1 ~finalize_pos:0;
  run ~feed_pos:0 ~finalize_pos:1;
  run ~feed_pos:1 ~finalize_pos:1;
  [%expect
    {|
    feed pos=0, finalize pos=0: whole="abc"; group 1="b"
    feed pos=1, finalize pos=0: whole="abc"; group 1="b"
    feed pos=0, finalize pos=1: whole="abc"; group 1="b"
    feed pos=1, finalize pos=1: whole="abc"; group 1="b"
    |}]
;;
