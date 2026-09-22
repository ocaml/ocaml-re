open Import

module type Str_intf = module type of Str

let implementations =
  [ "Str", (module Str : Str_intf); "Re.Str", (module Re.Str : Str_intf) ]
;;

let%expect_test "partial matches update the match state" =
  List.iter implementations ~f:(fun (name, (module S : Str_intf)) ->
    (* A failed match clears any captures left by another expect test. *)
    assert (not (S.string_match (S.regexp "z") "" 0));
    printf "%s: matched=%b; " name (S.string_partial_match (S.regexp "abc") "ab" 0);
    match S.match_end () with
    | pos -> printf "end=%d\n" pos
    | exception exn -> printf "%s\n" (Printexc.to_string exn));
  [%expect
    {|
    Str: matched=true; end=2
    Re.Str: matched=true; end=2
    |}]
;;

let%expect_test "descending Str ranges denote the empty character set" =
  List.iter implementations ~f:(fun (name, (module S : Str_intf)) ->
    printf "%s: %b\n" name (S.string_match (S.regexp "[z-a]") "z" 0));
  [%expect
    {|
    Str: false
    Re.Str: false
    |}]
;;

let%test_unit "descending Str ranges agree on every byte" =
  (* Keep the expanded native-Str oracle matrix on its original backend. *)
  match Sys.backend_type with
  | Other _ -> ()
  | Native | Bytecode ->
    List.iter [ "[z-a]"; "[^z-a]"; "[mz-a]"; "[z-z]" ] ~f:(fun pattern ->
      let expected = Str.regexp pattern in
      let actual = Re.Str.regexp pattern in
      for code = 0 to 255 do
        let s = String.make 1 (Char.chr code) in
        assert (
          Bool.equal (Str.string_match expected s 0) (Re.Str.string_match actual s 0))
      done)
;;
