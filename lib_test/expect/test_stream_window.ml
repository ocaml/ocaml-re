module Re = Re_private.Re
module Stream = Re.Stream

let report name f =
  match f () with
  | text -> Printf.printf "%s: %s\n" name text
  | exception Invalid_argument msg -> Printf.printf "%s: Invalid_argument %S\n" name msg
  | exception exn -> Printf.printf "%s: %s\n" name (Printexc.to_string exn)
;;

let%expect_test "stream entry points validate their input window" =
  let text = "a" in
  let fresh () = Stream.create (Re.compile Re.(str "a")) in
  let fresh_group () = Stream.Group.create (fresh ()) in
  List.iter
    (fun (pos, len) ->
       let suffix = Printf.sprintf "~pos:%d ~len:%d" pos len in
       report ("Stream.feed " ^ suffix) (fun () ->
         match Stream.feed (fresh ()) text ~pos ~len with
         | Ok _ -> "Ok"
         | No_match -> "No_match");
       report ("Stream.finalize " ^ suffix) (fun () ->
         string_of_bool (Stream.finalize (fresh ()) text ~pos ~len));
       report ("Stream.Group.feed " ^ suffix) (fun () ->
         match Stream.Group.feed (fresh_group ()) text ~pos ~len with
         | Ok _ -> "Ok"
         | No_match -> "No_match");
       report ("Stream.Group.finalize " ^ suffix) (fun () ->
         match Stream.Group.finalize (fresh_group ()) text ~pos ~len with
         | Ok _ -> "Ok"
         | No_match -> "No_match"))
    [ 0, -1; -1, 0; 2, 0; 2, 1 ];
  [%expect
    {|
    Stream.feed ~pos:0 ~len:-1: Invalid_argument "Re.Stream: out of bounds"
    Stream.finalize ~pos:0 ~len:-1: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.feed ~pos:0 ~len:-1: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.finalize ~pos:0 ~len:-1: Invalid_argument "Re.Stream: out of bounds"
    Stream.feed ~pos:-1 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.finalize ~pos:-1 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.feed ~pos:-1 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.finalize ~pos:-1 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.feed ~pos:2 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.finalize ~pos:2 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.feed ~pos:2 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.finalize ~pos:2 ~len:0: Invalid_argument "Re.Stream: out of bounds"
    Stream.feed ~pos:2 ~len:1: Invalid_argument "Re.Stream: out of bounds"
    Stream.finalize ~pos:2 ~len:1: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.feed ~pos:2 ~len:1: Invalid_argument "Re.Stream: out of bounds"
    Stream.Group.finalize ~pos:2 ~len:1: Invalid_argument "Re.Stream: out of bounds"
    |}]
;;
