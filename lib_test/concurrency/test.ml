module Barrier = struct
  type t =
    { waiters : int Atomic.t
    ; size : int
    ; passed : int Atomic.t
    }

  let create n = { waiters = Atomic.make n; size = n; passed = Atomic.make 0 }

  let await { waiters; size; passed } =
    if Atomic.fetch_and_add passed 1 = size - 1
    then (
      Atomic.set passed 0;
      Atomic.set waiters 0);
    while Atomic.get waiters = size do
      Domain.cpu_relax ()
    done;
    Atomic.incr waiters;
    while Atomic.get waiters < size do
      Domain.cpu_relax ()
    done
  ;;
end

let shuffle_array a =
  let n = Array.length a in
  let a' = Array.copy a in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = a'.(i) in
    a'.(i) <- a'.(j);
    a'.(j) <- temp
  done;
  a'
;;

let inverse_permutation p =
  let n = Array.length p in
  let inv = Array.make n 0 in
  for i = 0 to n - 1 do
    inv.(p.(i)) <- i
  done;
  inv
;;

let apply_permutation p a =
  let n = Array.length p in
  let b = Array.make n a.(0) in
  for i = 0 to n - 1 do
    b.(i) <- a.(p.(i))
  done;
  b
;;

(****)

let re1 = Re.(alt [ group (char 'a'); char 'b' ])
let re2 = Re.(seq [ re1; re1 ])
let re3 = Re.(seq [ re2; re2 ])
let re4 = Re.(seq [ re3; re3 ])

let re5 =
  Re.(
    alt
      [ seq [ re4; re4 ]
      ; group (str "b")
      ; group (str "bb")
      ; group (str "bbb")
      ; group (str "bbbb")
      ])
;;

let size = 300

let strings =
  Array.init size (fun _ -> String.init 30 (fun _ -> if Random.bool () then 'a' else 'b'))
;;

let execute ~short re a =
  apply_permutation
    (inverse_permutation a)
    (Array.map
       (fun i ->
         try
           Some
             (Re.Group.all_offset
              @@ Re.exec ~pos:(if short then 30 - 7 else 0) re strings.(i))
         with
         | Not_found -> None)
       a)
;;

let compare_groups g g' = g = g'

let concurrent f f' =
  let barrier = Barrier.create 2 in
  let domain =
    Domain.spawn
    @@ fun () ->
    Barrier.await barrier;
    f' ()
  in
  Barrier.await barrier;
  let res = f () in
  let res' = Domain.join domain in
  res, res'
;;

let sequential f f' = f (), f' ()

let test compose ~short n =
  let success = ref true in
  for _ = 1 to n do
    let re = Re.compile re5 in
    let a = shuffle_array (Array.init size Fun.id) in
    let a' = shuffle_array a in
    try
      let groups, groups' =
        compose (fun () -> execute ~short re a) (fun () -> execute ~short re a')
      in
      let ok = Array.for_all2 (Option.equal compare_groups) groups groups' in
      success := !success && ok;
      if not ok then prerr_endline "Bad group"
    with
    | Invalid_argument msg ->
      prerr_endline ("Invalid_argument " ^ msg);
      success := false
    | Division_by_zero ->
      prerr_endline "Division_by_zero";
      success := false
  done;
  if not !success then exit 1
;;

let () =
  prerr_endline "Sequential";
  test sequential ~short:false 20;
  test sequential ~short:true 10;
  prerr_endline "Concurrent";
  test ~short:false concurrent 750;
  test ~short:true concurrent 250
;;
