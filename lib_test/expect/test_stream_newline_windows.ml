open Import
module Stream = Re.Stream

let ok = function
  | Stream.Ok x -> x
  | No_match -> failwith "unexpected No_match"
;;

let%test_unit "final newlines across chunk splits, source offsets and empty feeds" =
  List.iter
    Re.
      [ seq [ str "a"; leol ]
      ; seq [ bos; str "a"; leol ]
      ; seq [ group (rep any); leol ]
      ; seq [ char '\n'; leol ]
      ; seq [ str "a"; leol; char '\n' ]
      ]
    ~f:(fun ast ->
      let re = Re.compile ast in
      List.iter [ ""; "a"; "a\n"; "a\nb"; "a\n\n"; "\n"; "xa\n"; "a\nb\n" ] ~f:(fun s ->
        let expected = Re.exec_opt re s in
        for split = 0 to String.length s do
          let first = "_" ^ String.sub s 0 split ^ "_" in
          let len = String.length s - split in
          let last = "_" ^ String.sub s split len ^ "_" in
          let actual =
            match Stream.feed (Stream.create re) first ~pos:1 ~len:split with
            | No_match -> false
            | Ok t ->
              let t = ok (Stream.feed t "" ~pos:0 ~len:0) in
              Stream.finalize t last ~pos:1 ~len
          in
          assert (Bool.equal (Option.is_some expected) actual);
          let actual =
            let stream = Stream.Group.create (Stream.create re) in
            match Stream.Group.feed stream first ~pos:1 ~len:split with
            | No_match -> Stream.No_match
            | Ok t ->
              let t = ok (Stream.Group.feed t "" ~pos:0 ~len:0) in
              Stream.Group.finalize t last ~pos:1 ~len
          in
          match expected, actual with
          | None, No_match -> ()
          | Some expected, Ok actual ->
            for i = 0 to Re.group_count re - 1 do
              assert (
                Option.equal
                  String.equal
                  (Re.Group.get_opt expected i)
                  (Stream.Group.Match.get actual i))
            done
          | _ -> failwith "stream/exec match disagreement"
        done))
;;
