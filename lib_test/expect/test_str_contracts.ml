module Re = Re_private.Re

let outcome f =
  match f () with
  | value -> Ok value
  | exception Not_found -> Error "Not_found"
  | exception Invalid_argument _ -> Error "Invalid_argument"
  | exception Failure _ -> Error "Failure"
;;

module Exercise (S : module type of Str) = struct
  let searches () =
    List.concat_map
      (fun pattern ->
         let re = S.regexp pattern in
         List.concat_map
           (fun input ->
              List.init
                (String.length input + 1)
                (fun pos ->
                   outcome (fun () ->
                     let found = S.search_backward re input pos in
                     found, S.match_beginning (), S.match_end (), S.matched_string input)))
           [ ""; "a"; "b"; "aba"; "xaa" ])
      [ "a"; "ab"; "a*"; ""; {|\(a\)?b|} ]
  ;;

  let partial () =
    List.concat_map
      (fun pattern ->
         List.concat_map
           (fun input ->
              List.init
                (String.length input + 1)
                (fun pos -> S.string_partial_match (S.regexp pattern) input pos))
           [ ""; "a"; "ab"; "abc"; "x" ])
      [ "ab"; "^ab"; "a*"; "" ]
  ;;

  (* Compare templates whose referenced groups participate. Substitution of
     absent groups is a separate compatibility issue, not an equality law. *)
  let replacements () =
    List.concat_map
      (fun (pattern, input) ->
         let re = S.regexp pattern in
         List.map
           (fun replacement ->
              ( pattern
              , input
              , replacement
              , outcome (fun () -> S.replace_first re replacement input)
              , outcome (fun () -> S.global_replace re replacement input) ))
           [ "X"; ""; {|\\|}; {|\0|}; {|\1|}; {|\q|}; {|\|} ])
      [ {|\(a\)|}, "aba"
      ; {|\(a\)?b|}, "ab ab"
      ; {|\(\)|}, ""
      ; {|\(a*\)|}, "aba"
      ; {|\(x\)|}, "aba"
      ]
  ;;

  let callbacks () =
    let re = S.regexp {|\(a\)|} in
    List.map
      (fun input ->
         let calls = ref [] in
         let f text =
           calls := (S.match_beginning (), S.matched_group 1 text) :: !calls;
           "[" ^ S.matched_string text ^ "]"
         in
         let first = S.substitute_first re f input in
         let first_calls = List.rev !calls in
         calls := [];
         let all = S.global_substitute re f input in
         first, first_calls, all, List.rev !calls)
      [ ""; "x"; "aba" ]
  ;;

  let groups () =
    let re = S.regexp {|\(a\)?\(b\)|} in
    List.map
      (fun input ->
         assert (S.string_match re input 0);
         ( List.init 3 (fun i ->
             ( outcome (fun () -> S.group_beginning i)
             , outcome (fun () -> S.group_end i)
             , outcome (fun () -> S.matched_group i input) ))
         , S.replace_matched {|[\0][\2]|} input ))
      [ "ab"; "b" ]
  ;;

  let splits () =
    let normalize =
      List.map (function
        | S.Text s -> `Text s
        | S.Delim s -> `Delim s)
    in
    List.concat_map
      (fun pattern ->
         let re = S.regexp pattern in
         List.concat_map
           (fun input ->
              List.map
                (fun limit ->
                   ( S.bounded_split re input limit
                   , S.bounded_split_delim re input limit
                   , normalize (S.bounded_full_split re input limit) ))
                [ 0; 1; 2; 3 ])
           [ ""; ","; ",a,,b,"; "a"; "aa" ])
      [ ","; ""; "a*"; "$" ]
  ;;

  let substrings () =
    List.concat_map
      (fun input ->
         List.init
           (String.length input + 3)
           (fun i ->
              let n = i - 1 in
              ( outcome (fun () -> S.string_before input n)
              , outcome (fun () -> S.string_after input n)
              , outcome (fun () -> S.first_chars input n)
              , outcome (fun () -> S.last_chars input n) )))
      [ ""; "abc" ]
  ;;
end

module Reference = Exercise (Str)
module Actual = Exercise (Re.Str)

let%expect_test "backward literal search and partial matching with independent oracles" =
  let re = Re.Str.regexp "a" in
  List.iter
    (fun input ->
       for pos = 0 to String.length input do
         let rec expected i =
           if i < 0
           then raise Not_found
           else if i < String.length input && input.[i] = 'a'
           then i
           else expected (i - 1)
         in
         assert (
           outcome (fun () -> Re.Str.search_backward re input pos)
           = outcome (fun () -> expected pos))
       done)
    [ ""; "a"; "b"; "aba" ];
  let re = Re.Str.regexp "ab" in
  List.iter
    (fun (input, expected) -> assert (Re.Str.string_partial_match re input 0 = expected))
    [ "", true; "a", true; "ab", true; "abc", true; "x", false ];
  [%expect {| |}]
;;

(* js_of_ocaml's Str backward-search reference exhausts the heap on this
   matrix. Keep the differential oracle native-only; the literal oracle above
   also checks Re.Str on JavaScript. *)
let%expect_test "backward searches and partial matching agree with Str" =
  (match Sys.backend_type with
   | Other _ -> ()
   | Native | Bytecode ->
     assert (Actual.searches () = Reference.searches ());
     assert (Actual.partial () = Reference.partial ()));
  [%expect {| |}]
;;

let%expect_test "replacement templates and callbacks agree with Str" =
  List.iter2
    (fun (p, s, r, a, b) (_, _, _, c, d) ->
       if (a, b) <> (c, d)
       then (
         let show = function
           | Ok s -> Printf.sprintf "Ok %S" s
           | Error s -> "Error " ^ s
         in
         failwith
           (Printf.sprintf
              "%S %S %S: %s,%s vs %s,%s"
              p
              s
              r
              (show a)
              (show b)
              (show c)
              (show d))))
    (Actual.replacements ())
    (Reference.replacements ());
  List.iter2
    (fun (a, ac, aa, aac) (b, bc, bb, bbc) ->
       if (a, ac, aa, aac) <> (b, bc, bb, bbc)
       then (
         let show calls =
           String.concat ";" (List.map (fun (i, s) -> Printf.sprintf "%d:%S" i s) calls)
         in
         failwith
           (Printf.sprintf
              "callbacks: actual %S [%s] %S [%s]; reference %S [%s] %S [%s]"
              a
              (show ac)
              aa
              (show aac)
              b
              (show bc)
              bb
              (show bbc))))
    (Actual.callbacks ())
    (Reference.callbacks ());
  assert (Actual.groups () = Reference.groups ());
  [%expect {| |}]
;;

let%expect_test "bounded and zero-width splits and substring helpers agree with Str" =
  assert (Actual.splits () = Reference.splits ());
  assert (Actual.substrings () = Reference.substrings ());
  [%expect {| |}]
;;

let%expect_test "literal regexps quote all byte values" =
  let check input =
    let re = Re.Str.regexp_string input in
    assert (Re.Str.string_match re input 0);
    assert (Re.Str.matched_string input = input);
    assert (Re.Str.match_end () = String.length input);
    assert (Re.Str.quote input = Str.quote input)
  in
  check "";
  for byte = 0 to 255 do
    check (String.make 1 (Char.chr byte))
  done;
  check (String.init 256 Char.chr);
  assert (Re.Str.string_match (Re.Str.regexp_string_case_fold "a[B]") "A[b]" 0);
  assert (not (Re.Str.string_match (Re.Str.regexp_string "a[B]") "A[b]" 0));
  [%expect {| |}]
;;
