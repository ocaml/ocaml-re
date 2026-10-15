open Import
open Re

let same_ast pattern expected =
  let show r = Format.asprintf "%a" pp r in
  assert (String.equal (show (Perl.re pattern)) (show expected))
;;

let%expect_test "a quantifier applies only to the last byte of a literal run" =
  List.iter [ 1; 2; 32; 4096 ] ~f:(fun len ->
    let prefix = String.make (len - 1) 'a' in
    List.iter
      [ "*", rep
      ; "+", rep1
      ; "?", opt
      ; ("{2}", fun r -> repn r 2 (Some 2))
      ; ("{2,3}", fun r -> repn r 2 (Some 3))
      ; ("{2,}", fun r -> repn r 2 None)
      ]
      ~f:(fun (suffix, quantify) ->
        List.iter
          [ "", greedy; "?", non_greedy ]
          ~f:(fun (modifier, semantics) ->
            let expected =
              if len = 1
              then semantics (quantify (char 'b'))
              else seq [ str prefix; semantics (quantify (char 'b')) ]
            in
            same_ast (prefix ^ "b" ^ suffix ^ modifier) expected)));
  [%expect {||}]
;;

let%expect_test "raw runs do not absorb escapes, groups or quoted atoms" =
  same_ast "ab\\Qcd\\E*ef" (seq [ str "ab"; char 'c'; greedy (rep (char 'd')); str "ef" ]);
  same_ast "ab(cd)ef" (seq [ str "ab"; group (str "cd"); str "ef" ]);
  same_ast "ab|cd" (alt [ str "ab"; str "cd" ]);
  same_ast "ab\\x41cd" (seq [ str "ab"; char 'A'; str "cd" ]);
  same_ast "ab]}cd" (str "ab]}cd");
  [%expect {||}]
;;

let%expect_test "comments and empty quotes do not detach a quantifier" =
  same_ast "abc(?#note)*" (seq [ str "ab"; greedy (rep (char 'c')) ]);
  same_ast "abc(?#note){2}" (seq [ str "ab"; greedy (repn (char 'c') 2 (Some 2)) ]);
  same_ast "abc\\Q\\E*" (seq [ str "ab"; greedy (rep (char 'c')) ]);
  [%expect {||}]
;;

let%expect_test "all ordinary bytes can occur inside literal runs" =
  let special = ".()|^$[\\*+?{" in
  let bytes =
    List.init ~len:256 ~f:Char.chr
    |> List.filter ~f:(fun c -> not (String.contains special c))
    |> List.to_seq
    |> String.of_seq
  in
  same_ast bytes (str bytes);
  [%expect {||}]
;;

let%expect_test "large runs produce a flat sequence without using the call stack" =
  let len = 100_000 in
  (match View.view (Perl.re (String.make len 'a')) with
   | Sequence chars -> assert (List.length chars = len)
   | _ -> assert false);
  [%expect {||}]
;;
