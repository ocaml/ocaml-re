module Re = Re_private.Re
module Pcre = Re.Pcre

let not_found f =
  match f () with
  | exception Not_found -> ()
  | _ -> failwith "expected Not_found"
;;

let%expect_test "PCRE flag forwarding and result entry points" =
  List.iter
    (fun (flags, pattern, input, expected) ->
       assert (Pcre.pmatch ~rex:(Pcre.regexp ~flags pattern) input = expected);
       match Pcre.re_result ~flags pattern with
       | Error _ -> assert false
       | Ok re -> assert (Re.execp (Re.compile re) input = expected))
    [ [ `CASELESS ], "abc", "ABC", true
    ; [], "abc", "ABC", false
    ; [ `MULTILINE ], "^a$", "x\na\ny", true
    ; [], "^a$", "x\na\ny", false
    ; [ `ANCHORED ], "a", "xa", false
    ; [], "a", "xa", true
    ; [ `DOTALL ], ".", "\n", true
    ; [], ".", "\n", false
    ; [ `CASELESS; `MULTILINE; `ANCHORED; `DOTALL ], "^a.b$", "A\nB", true
    ];
  assert (Pcre.re_result "(" = Error `Parse_error);
  assert (Pcre.re_result {|\1|} = Error `Not_supported);
  [%expect {| |}]
;;

let%expect_test "PCRE captures and duplicate names" =
  let rex = Pcre.regexp "(?<x>a)|(?<x>b)(?<empty>c?)" in
  assert (Pcre.names rex = [| "x"; "x"; "empty" |]);
  assert (Pcre.extract ~rex "b" = [| "b"; ""; "b"; "" |]);
  let g = Pcre.exec ~rex ~pos:1 "xb" in
  assert (Pcre.get_substring g 0 = "b");
  assert (Pcre.get_substring_ofs g 0 = (1, 2));
  assert (Pcre.get_named_substring_opt rex "x" g = Some "b");
  assert (Pcre.get_named_substring rex "empty" g = "");
  assert (Pcre.get_named_substring_opt rex "missing" g = None);
  not_found (fun () -> Pcre.get_named_substring rex "missing" g);
  not_found (fun () -> Pcre.get_substring g 1);
  not_found (fun () -> Pcre.get_substring_ofs g 1);
  let g = Pcre.exec ~rex "a" in
  assert (Pcre.get_named_substring_opt rex "empty" g = None);
  not_found (fun () -> Pcre.get_named_substring rex "empty" g);
  not_found (fun () -> Pcre.extract ~rex "z");
  [%expect {| |}]
;;

let%expect_test "quoting every byte produces a literal whole-string regex" =
  let check input =
    let re = Pcre.quote input |> Pcre.re |> Re.whole_string |> Re.compile in
    assert (Re.execp re input);
    assert (not (Re.execp re (input ^ "x")))
  in
  check "";
  for i = 0 to 255 do
    check (String.make 1 (Char.chr i))
  done;
  check (String.init 256 Char.chr);
  [%expect {| |}]
;;
