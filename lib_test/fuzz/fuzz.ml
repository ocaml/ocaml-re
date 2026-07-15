module List = Stdlib.ListLabels

module C = struct
  include Crowbar

  let check_eq' ~pp_ctx ~pp ~eq a b =
    let printed = ref false in
    let pp f a =
      if not !printed
      then (
        printed := true;
        pp_ctx f);
      pp f a
    in
    check_eq ~pp ~eq a b
  ;;

  let pp_array pp_a f a = pp_list pp_a f (Array.to_list a)
  let pp_pair pp1 pp2 f (a, b) = Format.fprintf f "@[<2>(%a,@ %a)@]" pp1 a pp2 b
end

let compare_descending compare a b = compare b a

module Ctx = struct
  type t =
    { counter : int ref
    ; nested_stars : int
    }
end

let char_gen =
  (* No reason to generate all 256 chars: except for a few chars referenced by built-in
     categories like \n or letters, chars are indistinguishable. A small charset
     reduces the reduces search space. The specific choice of chars is a bit arbitrary,
     but we want enough to cover eow or eol. We also only use ascii, so don't test
     anything involving latin1, which hardly anyone should care about these days. *)
  C.map
    [ C.range 10 ]
    (function
      | 0 -> '\n'
      | 1 -> 'a'
      | 2 -> 'b'
      | 3 -> 'A'
      | 4 -> 'B'
      | 5 -> '0'
      | 6 -> ' '
      | 7 -> '/'
      | 8 -> ':'
      | 9 -> '.'
      | _ -> assert false)
;;

let string_of_chars chars =
  let chars = Array.of_list chars in
  String.init (Array.length chars) (fun i -> chars.(i))
;;

let rec string_gen n =
  if n = 0
  then C.const ""
  else if n = 1
  then C.map [ char_gen ] (fun c -> string_of_chars [ c ])
  else C.map [ string_gen (n / 2); string_gen (n - (n / 2)) ] ( ^ )
;;

let string_gen n = C.with_printer C.pp_string (string_gen n)

let string_gen_dyn ?(min = 0) n =
  C.with_printer
    C.pp_string
    (C.dynamic_bind (C.range (n - min)) (fun n -> string_gen (min + n)))
;;

let group_name n = Printf.sprintf "%03d" n

let re_gen =
  (* This doesn't cover every construction of regular expression: the point is to
     test the execution engine, so testing intersection of character set, or no group,
     is not very important. *)
  C.with_printer
    (fun fmt (_, re) -> Re.pp fmt re)
    (C.map
       [ C.fix (fun self ->
           C.choose
             [ C.map
                 [ C.list char_gen ]
                 (fun chars _ctx -> Re.set (string_of_chars chars))
             ; C.map
                 [ C.list self ]
                 (fun rs ctx -> Re.alt (List.map rs ~f:(fun r -> r ctx)))
             ; C.map
                 [ C.list self ]
                 (fun rs ctx -> Re.seq (List.map rs ~f:(fun r -> r ctx)))
             ; C.map [ self; C.bool ] (fun r greedy (ctx : Ctx.t) ->
                 C.guard (ctx.nested_stars <= 1);
                 (* I don't imagine nested stars add much in terms of coverage, and they
                    risk making the backtracking implementation explode. Well I suppose
                    sequential stars have the same problem, so maybe we should limit total
                    stars. *)
                 let r = Re.rep (r { ctx with nested_stars = ctx.nested_stars + 1 }) in
                 if greedy then Re.greedy r else Re.non_greedy r)
             ; C.map [ self ] (fun r (ctx : Ctx.t) ->
                 let name =
                   (* Names don't influence behavior, so we force specific names instead
                      of wasting fuzzing time on different names. *)
                   ctx.counter := !(ctx.counter) + 1;
                   group_name !(ctx.counter)
                 in
                 Re.group ~name (r ctx))
             ; C.map
                 [ C.range 3; self ]
                 (fun n r ctx ->
                   match n with
                   | 0 -> Re.shortest (r ctx)
                   | 1 -> Re.longest (r ctx)
                   | 2 -> Re.first (r ctx)
                   | _ -> assert false)
             ; C.choose
                 [ C.const (fun (_ctx : Ctx.t) -> Re.bol)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.eol)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.bos)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.eos)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.bow)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.eow)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.start)
                 ; C.const (fun (_ctx : Ctx.t) -> Re.stop)
                 ]
             ])
       ]
       (fun f ->
         let group_counter = ref 0 in
         group_counter, f { counter = group_counter; nested_stars = 0 }))
;;

module Compare_to_reference = struct
  type ctx =
    { str : string
    ; start : int
    ; stop : int
    ; rep : Re.View.Rep_kind.t
    }

  module String_map = Map.Make (String)

  type state =
    { pos : int
    ; matches : (int * int) String_map.t
    }

  let pp_state f { pos; matches } =
    Format.fprintf
      f
      "@[{@ pos:@ %d;@ matches:@ [@,%a@,]@ }@]"
      pos
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (str, (a, b)) -> Format.fprintf fmt "%s:%d:%d" str a b))
      (String_map.to_seq matches |> List.of_seq)
  ;;

  let pp_states fmt states =
    Format.fprintf
      fmt
      "[@[%a@]]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") pp_state)
      states
  ;;

  let peek_ahead ctx pos =
    if pos < 0 || pos >= String.length ctx.str then None else Some ctx.str.[pos]
  ;;

  let peek_behind ctx pos = peek_ahead ctx (pos - 1)

  let consume_byte ctx pos =
    if pos < ctx.start || pos >= ctx.stop then None else Some (ctx.str.[pos], pos + 1)
  ;;

  let wordc = function
    | None -> None
    | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') -> Some true
    | Some _ -> Some false
  ;;

  let rec fold_left f acc l k =
    match l with
    | [] -> k acc
    | hd :: tl -> f acc hd (fun acc -> fold_left f acc tl k)
  ;;

  let find_success (type a) f =
    let exception E of a in
    match f (fun a -> raise_notrace (E a)) with
    | exception E a -> Some a
    | _ -> None
  ;;

  let reorder_matches ~pp_key f ~compare k =
    let matches = ref [] in
    f (fun compare_key state -> matches := (compare_key, state) :: !matches);
    List.rev !matches
    |> (fun l ->
         if false
         then
           Format.printf
             "@[<2>before reorder: %a@]@\n"
             (Format.pp_print_list
                ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
                (fun fmt (key, state) ->
                  Format.fprintf fmt "(%a,%a)" pp_key key pp_state state))
             l;
         l)
    |> List.stable_sort ~cmp:(fun (a, _) (b, _) -> compare a b)
    |> List.map ~f:snd
    |> List.iter ~f:k
  ;;

  let debug r _ctx state k f =
    if true
    then f k
    else (
      match Re.View.view r with
      | Set _
      | Beg_of_line
      | End_of_line
      | Beg_of_word
      | End_of_word
      | Not_bound
      | Beg_of_str
      | End_of_str
      | Sem _ -> f k
      | _ ->
        let matches = ref [] in
        f (fun state -> matches := state :: !matches);
        let matches = List.rev !matches in
        Format.printf "@[<2>%a@ %a:@ %a@]@\n" Re.pp r pp_state state pp_states matches;
        List.iter matches ~f:k)
  ;;

  let rec reference r ctx state k =
    debug r ctx state k (fun k ->
      match Re.View.view r with
      | Set cset ->
        (match consume_byte ctx state.pos with
         | Some (c, pos')
           when let module Cset = Re.View.Cset in
                List.exists (Cset.view cset) ~f:(fun range ->
                  Cset.Range.first range <= c && c <= Cset.Range.last range) ->
           k { matches = state.matches; pos = pos' }
         | _ -> ())
      | Sequence rs -> fold_left (fun state r k -> reference r ctx state k) state rs k
      | Alternative rs -> List.iter rs ~f:(fun r -> reference r ctx state k)
      | Repeat (r1, min, max) ->
        assert (min = 0);
        assert (Option.is_none max);
        let ordering =
          match ctx.rep with
          | `Non_greedy -> Fun.id
          | `Greedy -> compare_descending
        in
        reorder_matches
          ~pp_key:(fun fmt n -> Format.fprintf fmt "prio:%d" n)
          ~compare:(ordering Int.compare)
          (fun k ->
            k 0 state;
            reference r1 ctx state (fun state2 ->
              k (if state.pos = state2.pos then 1 else 2) state2))
          (fun state2 ->
            if state.pos = state2.pos then k state2 else reference r ctx state2 k)
      | Beg_of_line ->
        (match peek_behind ctx state.pos with
         | Some '\n' | None -> k state
         | _ -> ())
      | End_of_line ->
        (match peek_ahead ctx state.pos with
         | Some '\n' | None -> k state
         | _ -> ())
      | Beg_of_word ->
        (match wordc (peek_behind ctx state.pos), wordc (peek_ahead ctx state.pos) with
         | (None | Some false), Some true -> k state
         | _ -> ())
      | End_of_word ->
        (match wordc (peek_behind ctx state.pos), wordc (peek_ahead ctx state.pos) with
         | Some true, (None | Some false) -> k state
         | _ -> ())
      | Not_bound -> assert false
      | Beg_of_str ->
        (match peek_behind ctx state.pos with
         | None -> k state
         | Some _ -> ())
      | End_of_str ->
        (match peek_ahead ctx state.pos with
         | None -> k state
         | Some _ -> ())
      | Last_end_of_line -> assert false
      | Start -> if state.pos = ctx.start then k state
      | Stop -> if state.pos = ctx.stop then k state
      | Sem_greedy (rep, r) -> reference r { ctx with rep } state k
      | Sem (sem, r) ->
        (match sem with
         | `First -> reference r ctx state k
         | (`Shortest | `Longest) as sem ->
           reorder_matches
             ~pp_key:(fun fmt pos -> Format.fprintf fmt "pos:%d" pos)
             ~compare:
               (match sem with
                | `Shortest -> Int.compare
                | `Longest -> compare_descending Int.compare)
             (fun k -> reference r ctx state (fun state -> k state.pos state))
             k)
      | Group (Some s, r) ->
        (* We can't really handle numbered groups (we'd need a first pass to number the
           groups, but we'd have nowhere to store the numbers), so we instead require
           groups be named. *)
        reference r ctx state (fun state2 ->
          k
            { state2 with
              matches = String_map.add s (state.pos, state2.pos) state2.matches
            })
      | Group (None, _) -> assert false
      | No_group _ -> assert false
      | Nest _ -> assert false
      | Case _ -> assert false
      | No_case _ -> assert false
      | Intersection _ -> assert false
      | Complement _ -> assert false
      | Difference _ -> assert false
      | Pmark _ -> assert false)
  ;;

  let exec_opt re ?(pos = 0) ?(len = -1) str =
    let start = pos in
    let stop = if len = -1 then String.length str else start + len in
    assert (0 <= start);
    assert (start <= stop);
    assert (stop <= String.length str);
    find_success
      (reference
         (Re.seq [ Re.non_greedy (Re.rep Re.any); Re.group ~name:(group_name 0) re ])
         { str; start; stop; rep = `Greedy }
         { pos; matches = String_map.empty })
    |> Option.map (fun state -> state.matches)
  ;;

  let all_offset group_counter matches =
    Option.map
      (fun m ->
        Array.init (group_counter + 1) (fun n ->
          match String_map.find_opt (group_name n) m with
          | None -> -1, -1
          | Some p -> p))
      matches
  ;;

  let same_execution (group_counter, re) ?pos ?len input =
    let res1 = exec_opt re input ?pos ?len in
    let res1_list = all_offset !group_counter res1 in
    let res2 = Re.exec_opt (Re.compile re) input ?pos ?len in
    let res2_list = Option.map (fun group -> Re.Group.all_offset group) res2 in
    C.check_eq'
      ~eq:(Stdlib.( = ) : (int * int) array option -> _)
      res1_list
      res2_list
      ~pp:(C.pp_option (C.pp_array (C.pp_pair C.pp_int C.pp_int)))
      ~pp_ctx:(fun f ->
        Option.iter (Format.fprintf f "pos: %d@\n") pos;
        Option.iter (Format.fprintf f "len: %d@\n") len;
        match pos, len with
        | Some pos, Some len ->
          Format.fprintf f "input range: %S@\n" (String.sub input pos len)
        | _, None | None, _ -> ())
  ;;

  let add_test () =
    (* As of writing, about 13s for 1M tests, 10min for 50M. *)
    C.add_test
      ~name:"compare_to_reference"
      [ re_gen; string_gen_dyn 6 ]
      (fun re input -> same_execution re input);
    C.add_test
      ~name:"compare_to_reference_sub"
      [ re_gen; string_gen_dyn ~min:2 6 ]
      (fun re input -> same_execution re input ~pos:1 ~len:(String.length input - 2));
    ()
  ;;

  let () =
    if false
    then (
      let group_counter = ref 0 in
      let manual_test re ?pos ?len str =
        let res1 = exec_opt re ?pos ?len str in
        let res1_list = all_offset !group_counter res1 in
        Format.printf
          "%a@."
          (C.pp_option (fun f a ->
             C.pp_list
               (fun f (a, b) -> Format.fprintf f "@[<2>(%a,@ %a)@]" C.pp_int a C.pp_int b)
               f
               (Array.to_list a)))
          res1_list;
        failwith "stop"
      in
      let open Re in
      let group r =
        group_counter := !group_counter + 1;
        group ~name:(group_name !group_counter) r
      in
      let _ = group in
      manual_test (longest (seq [ group (non_greedy (rep any)); rep any ])) "a")
  ;;
end

module Exec_partial = struct
  let add_test () =
    C.add_test
      ~name:"exec_partial"
      [ re_gen; string_gen_dyn 6; string_gen_dyn 6 ]
      (fun (_, re) prefix rest ->
        let re = Re.compile re in
        match Re.exec_partial_detailed re prefix with
        | `Partial n ->
          (match Re.exec_opt re (prefix ^ rest) with
           | None -> ()
           | Some group -> C.check (Re.Group.start group 0 >= n))
        | (`Full _ | `Mismatch) as res ->
          let res1 =
            match res with
            | `Full group -> Some (Re.Group.all_offset group)
            | `Mismatch -> None
          in
          let res2 = Re.exec_opt re (prefix ^ rest) |> Option.map Re.Group.all_offset in
          C.check_eq'
            ~eq:(Stdlib.( = ) : (int * int) array option -> _)
            res1
            res2
            ~pp:(C.pp_option (C.pp_array (C.pp_pair C.pp_int C.pp_int)))
            ~pp_ctx:(fun f -> Format.fprintf f "input: %S %S@\n" prefix rest))
  ;;
end

let () =
  Compare_to_reference.add_test ();
  if false then Exec_partial.add_test ()
;;

(* Currently, this fuzzing is run manually, it's not plugged into dune or CI or anything.
   It can be used either by just running the exe (in which case, it behaves as
   quickcheck, blindly generating values), or as:
   mkdir -p _build/input
   AFL_SKIP_CPUFREQ=1 afl-fuzz -i _build/input -o _build/output _build/default/lib_test/fuzz/fuzz.exe @@
*)
