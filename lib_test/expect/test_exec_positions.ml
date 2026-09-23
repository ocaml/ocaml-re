module Re = Re_private.Re

let report name f =
  match f () with
  | text -> Printf.printf "%s: %s\n" name text
  | exception Invalid_argument msg -> Printf.printf "%s: Invalid_argument %S\n" name msg
  | exception Not_found -> Printf.printf "%s: Not_found\n" name
  | exception exn -> Printf.printf "%s: %s\n" name (Printexc.to_string exn)
;;

let%expect_test "positions past the input are handled with omitted or negative lengths" =
  let re = Re.compile Re.(str "a") in
  let text = "a" in
  List.iter
    (fun (pos, len) ->
       let suffix =
         match len with
         | None -> Printf.sprintf "~pos:%d" pos
         | Some len -> Printf.sprintf "~pos:%d ~len:%d" pos len
       in
       report ("exec_opt " ^ suffix) (fun () ->
         match Re.exec_opt ?len re text ~pos with
         | None -> "None"
         | Some group -> Printf.sprintf "Some %S" (Re.Group.get group 0));
       report ("execp " ^ suffix) (fun () -> string_of_bool (Re.execp ?len re text ~pos));
       report ("all " ^ suffix) (fun () ->
         Printf.sprintf "%d matches" (List.length (Re.all ?len re text ~pos)));
       report ("split " ^ suffix) (fun () ->
         Printf.sprintf "%S" (String.concat "," (Re.split ?len re text ~pos))))
    [ 2, None; 2, Some (-1); 2, Some 0 ];
  [%expect
    {|
    exec_opt ~pos:2: Invalid_argument "Re.exec: out of bounds"
    execp ~pos:2: Invalid_argument "Re.exec: out of bounds"
    all ~pos:2: Invalid_argument "Re.all"
    split ~pos:2: Invalid_argument "Re.split"
    exec_opt ~pos:2 ~len:-1: Invalid_argument "Re.exec: out of bounds"
    execp ~pos:2 ~len:-1: Invalid_argument "Re.exec: out of bounds"
    all ~pos:2 ~len:-1: Invalid_argument "Re.all"
    split ~pos:2 ~len:-1: Invalid_argument "Re.split"
    exec_opt ~pos:2 ~len:0: Invalid_argument "Re.exec: out of bounds"
    execp ~pos:2 ~len:0: Invalid_argument "Re.exec: out of bounds"
    all ~pos:2 ~len:0: Invalid_argument "Re.all"
    split ~pos:2 ~len:0: Invalid_argument "Re.split"
    |}]
;;
