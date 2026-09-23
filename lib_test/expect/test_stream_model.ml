module Re = Re_private.Re

let partitions input =
  let rec loop pos =
    if pos = String.length input
    then [ [] ]
    else
      List.concat
        (List.init
           (String.length input - pos)
           (fun offset ->
              let len = offset + 1 in
              List.map (fun rest -> String.sub input pos len :: rest) (loop (pos + len))))
  in
  loop 0
;;

let%expect_test "stream results are independent of chunk boundaries and source offsets" =
  let mark, marked_pattern = Re.(mark (group (str "ab"))) in
  let cases =
    Re.
      [ marked_pattern
      ; whole_string marked_pattern
      ; seq [ str "xx"; marked_pattern ]
      ; group (rep (char 'a'))
      ; seq [ group (char 'a'); opt (group (char 'b')); group epsilon ]
      ; empty
      ]
  in
  List.iter
    (fun pattern ->
       let re = Re.compile pattern in
       List.iter
         (fun input ->
            let expected = Re.exec_opt re input in
            List.iter
              (fun chunks ->
                 List.iter
                   (fun padding ->
                      List.iter
                        (fun finalize_empty ->
                           let chunks =
                             "" :: List.concat_map (fun s -> [ ""; s ]) chunks
                           in
                           let chunks =
                             if finalize_empty then chunks @ [ "" ] else chunks
                           in
                           let source s = String.make padding 'z' ^ s ^ "y" in
                           let rec run plain grouped = function
                             | [] -> assert false
                             | [ last ] ->
                               let text = source last in
                               let matched =
                                 Re.Stream.finalize
                                   plain
                                   text
                                   ~pos:padding
                                   ~len:(String.length last)
                               in
                               assert (matched = Option.is_some expected);
                               (match
                                  ( Re.Stream.Group.finalize
                                      grouped
                                      text
                                      ~pos:padding
                                      ~len:(String.length last)
                                  , expected )
                                with
                                | No_match, None -> ()
                                | Ok actual, Some expected ->
                                  for i = 0 to Re.group_count re do
                                    assert (
                                      Re.Stream.Group.Match.get actual i
                                      = Re.Group.get_opt expected i)
                                  done;
                                  assert (
                                    Re.Stream.Group.Match.test_mark actual mark
                                    = Re.Mark.test expected mark)
                                | _ ->
                                  failwith
                                    "stream match disagrees with whole-string execution")
                             | chunk :: rest ->
                               let text = source chunk in
                               (match
                                  ( Re.Stream.feed
                                      plain
                                      text
                                      ~pos:padding
                                      ~len:(String.length chunk)
                                  , Re.Stream.Group.feed
                                      grouped
                                      text
                                      ~pos:padding
                                      ~len:(String.length chunk) )
                                with
                                | No_match, No_match -> assert (expected = None)
                                | Ok plain, Ok grouped -> run plain grouped rest
                                | _ -> failwith "marked and unmarked streams disagree")
                           in
                           let plain = Re.Stream.create re in
                           try run plain (Re.Stream.Group.create plain) chunks with
                           | exn ->
                             failwith
                               (Format.asprintf
                                  "pattern=%a input=%S chunks=[%s] padding=%d \
                                   finalize_empty=%b: %s"
                                  Re.pp
                                  pattern
                                  input
                                  (String.concat
                                     ";"
                                     (List.map (Printf.sprintf "%S") chunks))
                                  padding
                                  finalize_empty
                                  (Printexc.to_string exn)))
                        [ false; true ])
                   [ 0; 2 ])
              (partitions input))
         [ ""; "a"; "ab"; "xxab"; "aab"; "b" ])
    cases;
  [%expect {| |}]
;;
