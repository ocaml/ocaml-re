module Re = Re_private.Re
module Stream = Re.Stream

let%expect_test "streamed captures after discarding unmatched prefixes" =
  List.iter
    (fun input ->
       let re = Re.compile (Re.str "foo") in
       let whole = Re.Group.get (Re.exec re input) 0 in
       let stream = Stream.Group.create (Stream.create re) in
       let matched =
         match Stream.Group.finalize stream input ~pos:0 ~len:(String.length input) with
         | No_match -> failwith "expected a streaming match"
         | Ok matched -> matched
       in
       let capture =
         try
           match Stream.Group.Match.get matched 0 with
           | None -> "None"
           | Some text -> Printf.sprintf "%S" text
         with
         | exn -> Printexc.to_string exn
       in
       Printf.printf "%S: exec=%S; stream=%s\n" input whole capture)
    [ "foo"; "xfoo"; "xxxxfoo" ];
  (* All three captures should be "foo". Every source offset is zero and each
     stream is finalized only once, isolating the dropping of unmatched input
     prefixes from nonzero source offsets and terminal-state reuse. *)
  [%expect
    {|
    "foo": exec="foo"; stream="foo"
    "xfoo": exec="foo"; stream="foo"
    "xxxxfoo": exec="foo"; stream="foo"
    |}]
;;
