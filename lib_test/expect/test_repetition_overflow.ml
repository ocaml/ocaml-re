module Re = Re_private.Re

module type Tests = sig end

let report overflowing =
  let rec bounds re =
    match Re.View.view re with
    | Repeat (_, minimum, Some maximum) ->
      Printf.sprintf "bounds (%d, %d)" minimum maximum
    | Sem_greedy (_, re) -> bounds re
    | _ -> failwith "expected a bounded repetition"
  in
  let above_max_int = Int64.to_string (Int64.succ (Int64.of_int max_int)) in
  List.iter
    (fun count ->
       let pattern = "a{" ^ count ^ "}" in
       List.iter
         (fun (name, parse) ->
            let result =
              match parse pattern with
              | Ok re -> bounds re
              | Error `Parse_error -> "Parse_error"
              | Error `Not_supported -> "Not_supported"
            in
            Printf.printf "%s %S: %s\n" name pattern result)
         [ ("Perl", fun pattern -> Re.Perl.re_result pattern)
         ; ("Posix", fun pattern -> Re.Posix.re_result pattern)
         ])
    [ "10"; string_of_int max_int; above_max_int; overflowing ]
;;

(* Parse only: compiling enormous repetitions would allocate unnecessarily.
   Both out-of-range counts should be rejected. The final count currently
   wraps to a positive value large enough to evade the post-arithmetic check. *)
let%test_module "overflowing repetition counts" =
  (module (val if Sys.int_size = 63
               then
                 (module struct
                   let%expect_test "63-bit integers" =
                     report "11000000000000000000";
                     [%expect
                       {|
    Perl "a{10}": bounds (10, 10)
    Posix "a{10}": bounds (10, 10)
    Perl "a{4611686018427387903}": bounds (4611686018427387903, 4611686018427387903)
    Posix "a{4611686018427387903}": bounds (4611686018427387903, 4611686018427387903)
    Perl "a{4611686018427387904}": Parse_error
    Posix "a{4611686018427387904}": Parse_error
    Perl "a{11000000000000000000}": bounds (1776627963145224192, 1776627963145224192)
    Posix "a{11000000000000000000}": bounds (1776627963145224192, 1776627963145224192)
    |}]
                   ;;
                 end)
               else if Sys.int_size = 32
               then
                 (module struct
                   let%expect_test "JavaScript integers" =
                     report "5000000000";
                     [%expect
                       {|
    Perl "a{10}": bounds (10, 10)
    Posix "a{10}": bounds (10, 10)
    Perl "a{2147483647}": bounds (2147483647, 2147483647)
    Posix "a{2147483647}": bounds (2147483647, 2147483647)
    Perl "a{2147483648}": Parse_error
    Posix "a{2147483648}": Parse_error
    Perl "a{5000000000}": bounds (705032704, 705032704)
    Posix "a{5000000000}": bounds (705032704, 705032704)
    |}]
                   ;;
                 end)
               else (module struct end)
            : Tests))
;;
