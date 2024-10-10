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

let%expect_test "basic" =
  let re = [ Re.bos; Re.str "abab" ] |> Re.seq |> Re.compile in
  let s = Stream.create re in
  ignore (feed s "x");
  [%expect {| "x" did not match |}];
  let suffix = "ab" in
  let s =
    match feed s suffix with
    | Ok s -> s
    | No_match -> assert false
  in
  [%expect {|
    "ab" not matched (status = unmatched) |}];
  (let (_ : Stream.feed) = feed s "ab" in
   [%expect {|
     "ab" not matched (status = matched) |}]);
  let (_ : Stream.feed) = feed s "xy" in
  [%expect {|
    "xy" did not match |}]
;;
