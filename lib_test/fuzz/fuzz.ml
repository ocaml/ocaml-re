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
    ; partial_safe : bool
    }
end

let char_gen =
  (* No reason to generate all 256 chars: except for a few chars referenced by built-in
     categories like \n or letters, chars are indistinguishable. A small charset
     reduces the search space. The specific choice of chars is a bit arbitrary,
     but we want enough to cover eow or eol, plus one latin1 case pair to exercise
     [no_case] and the latin1 word categories. *)
  C.map
    [ C.range 12 ]
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
      | 10 -> '\192'
      | 11 -> '\224'
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

let cset_gen =
  (* Well-formed operands for Re.inter/Re.compl/Re.diff: character sets, case
     modifiers on sets, and nested set algebra. *)
  C.with_printer
    (fun fmt re -> Re.pp fmt re)
    (C.fix (fun self ->
       C.choose
         [ C.map [ C.list char_gen ] (fun chars -> Re.set (string_of_chars chars))
         ; C.map [ self ] (fun r -> Re.no_case r)
         ; C.map [ self ] (fun r -> Re.case r)
         ; C.map [ C.list self ] (fun rs -> Re.inter rs)
         ; C.map [ C.list self ] (fun rs -> Re.compl rs)
         ; C.map [ self; self ] (fun a b -> Re.diff a b)
         ]))
;;

let make_re_gen ~partial_safe =
  (* This covers the AST constructions the execution engine distinguishes,
     including bounded repetition, case modifiers, nest/no_group and set
     algebra. Marks and unnamed groups are still left out. *)
  C.with_printer
    (fun fmt (_, re) -> Re.pp fmt re)
    (C.map
       [ C.fix (fun self ->
           C.choose
             [ C.map [ cset_gen ] (fun r _ctx -> r)
             ; C.map
                 [ C.list self ]
                 (fun rs ctx -> Re.alt (List.map rs ~f:(fun r -> r ctx)))
             ; C.map
                 [ C.list self ]
                 (fun rs ctx -> Re.seq (List.map rs ~f:(fun r -> r ctx)))
             ; C.map
                 [ self; C.range 6; C.bool ]
                 (fun r shape greedy (ctx : Ctx.t) ->
                    C.guard (ctx.nested_stars <= 1);
                    (* I don't imagine nested repetitions add much in terms of coverage, and
                    they risk making the backtracking implementation explode. Well I
                    suppose sequential repetitions have the same problem, so maybe we
                    should limit total repetitions. *)
                    let r = r { ctx with nested_stars = ctx.nested_stars + 1 } in
                    let rep =
                      if ctx.partial_safe
                      then Re.rep r
                      else (
                        match shape with
                        | 0 -> Re.rep r
                        | 1 -> Re.rep1 r
                        | 2 -> Re.repn r 0 (Some 1)
                        | 3 -> Re.repn r 1 (Some 2)
                        | 4 -> Re.repn r 2 (Some 4)
                        | 5 -> Re.repn r 0 (Some 3)
                        | _ -> assert false)
                    in
                    if greedy then Re.greedy rep else Re.non_greedy rep)
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
             ; C.map
                 [ self; C.range 3 ]
                 (fun r shape ctx ->
                    match shape with
                    | 0 -> Re.case (r ctx)
                    | 1 -> Re.no_case (r ctx)
                    | 2 -> Re.nest (r ctx)
                    | _ -> assert false)
             ; C.map [ self; C.bool ] (fun r hide ctx ->
                 if hide then Re.no_group (r ctx) else r ctx)
             ; C.choose
                 (let constants =
                    [ C.const (fun (_ctx : Ctx.t) -> Re.bol)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.eol)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.bos)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.eos)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.bow)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.eow)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.start)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.stop)
                    ; C.const (fun (_ctx : Ctx.t) -> Re.not_boundary)
                    ]
                  in
                  if partial_safe
                  then constants
                  else C.const (fun (_ctx : Ctx.t) -> Re.leol) :: constants)
             ])
       ]
       (fun f ->
          let group_counter = ref 0 in
          group_counter, f { counter = group_counter; nested_stars = 0; partial_safe }))
;;

let re_gen = make_re_gen ~partial_safe:false
let re_gen_partial = make_re_gen ~partial_safe:true

module Compare_to_reference = struct
  type ctx =
    { str : string
    ; start : int
    ; stop : int
    ; rep : Re.View.Rep_kind.t
    ; case_insens : bool
    ; no_capture : bool
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
    | Some
        ( 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '_' | '\170' | '\181' | '\186'
        | '\192' .. '\214'
        | '\216' .. '\246'
        | '\248' .. '\255' ) -> Some true
    | Some _ -> Some false
  ;;

  (* Mirrors [Category.from_char]. *)
  let is_upper = function
    | 'A' .. 'Z' | '\192' .. '\214' | '\216' .. '\222' -> true
    | _ -> false
  ;;

  let is_lower = function
    | 'a' .. 'z' | '\181' | '\223' .. '\246' | '\248' .. '\255' -> true
    | _ -> false
  ;;

  (* Membership in [Cset.case_insens s]. *)
  let cset_mem ~case_insens cset c =
    let module Cset = Re.View.Cset in
    let raw c =
      List.exists (Cset.view cset) ~f:(fun range ->
        Cset.Range.first range <= c && c <= Cset.Range.last range)
    in
    raw c
    || (case_insens
        && ((let d = Char.code c - 32 in
             d >= 0 && is_upper (Char.chr d) && raw (Char.chr d))
            ||
            let d = Char.code c + 32 in
            d < 256 && is_lower (Char.chr d) && raw (Char.chr d)))
  ;;

  let rec group_names r =
    match Re.View.view r with
    | Set _
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | End_of_str
    | Last_end_of_line
    | Start
    | Stop -> []
    | Sequence rs | Alternative rs | Intersection rs | Complement rs ->
      List.concat (List.map rs ~f:group_names)
    | Repeat (r, _, _)
    | Sem (_, r)
    | Sem_greedy (_, r)
    | No_group r
    | Nest r
    | Case r
    | No_case r
    | Pmark (_, r) -> group_names r
    | Difference (a, b) -> group_names a @ group_names b
    | Group (Some name, r) -> name :: group_names r
    | Group (None, r) -> group_names r
  ;;

  let rec view_matches_byte ctx r c =
    match Re.View.view r with
    | Set cset -> cset_mem ~case_insens:ctx.case_insens cset c
    | Case r -> view_matches_byte { ctx with case_insens = false } r c
    | No_case r -> view_matches_byte { ctx with case_insens = true } r c
    | Intersection rs -> List.for_all rs ~f:(fun r -> view_matches_byte ctx r c)
    | Complement rs -> not (List.exists rs ~f:(fun r -> view_matches_byte ctx r c))
    | Difference (a, b) -> view_matches_byte ctx a c && not (view_matches_byte ctx b c)
    | _ -> assert false
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
         | Some (c, pos') when cset_mem ~case_insens:ctx.case_insens cset c ->
           k { matches = state.matches; pos = pos' }
         | _ -> ())
      | Sequence rs -> fold_left (fun state r k -> reference r ctx state k) state rs k
      | Alternative rs -> List.iter rs ~f:(fun r -> reference r ctx state k)
      | Repeat (r1, min, max) ->
        let ordering =
          match ctx.rep with
          | `Non_greedy -> Fun.id
          | `Greedy -> compare_descending
        in
        let rec exact n state k =
          if n = 0
          then k state
          else reference r1 ctx state (fun state2 -> exact (n - 1) state2 k)
        in
        let rec optional remaining state k =
          if remaining = 0
          then k state
          else
            reorder_matches
              ~pp_key:(fun fmt n -> Format.fprintf fmt "prio:%d" n)
              ~compare:(ordering Int.compare)
              (fun k ->
                 k 0 state;
                 reference r1 ctx state (fun state2 -> k 1 state2))
              (fun state2 -> optional (remaining - 1) state2 k)
        in
        let rec star state k =
          reorder_matches
            ~pp_key:(fun fmt n -> Format.fprintf fmt "prio:%d" n)
            ~compare:(ordering Int.compare)
            (fun k ->
               k 0 state;
               reference r1 ctx state (fun state2 ->
                 k (if state.pos = state2.pos then 1 else 2) state2))
            (fun state2 -> if state.pos = state2.pos then k state2 else star state2 k)
        in
        let tail state =
          match max with
          | None -> star state k
          | Some max -> optional (max - min) state k
        in
        exact min state tail
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
      | Not_bound ->
        (match wordc (peek_behind ctx state.pos), wordc (peek_ahead ctx state.pos) with
         | (None | Some false), Some true | Some true, (None | Some false) -> ()
         | _ -> k state)
      | Beg_of_str ->
        (match peek_behind ctx state.pos with
         | None -> k state
         | Some _ -> ())
      | End_of_str ->
        (match peek_ahead ctx state.pos with
         | None -> k state
         | Some _ -> ())
      | Last_end_of_line ->
        let slen = String.length ctx.str in
        if
          state.pos >= slen
          || (state.pos = slen - 1 && Char.equal ctx.str.[state.pos] '\n')
        then k state
        else ()
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
          let matches =
            if ctx.no_capture
            then state2.matches
            else String_map.add s (state.pos, state2.pos) state2.matches
          in
          k { state2 with matches })
      | Group (None, _) -> assert false
      | No_group r -> reference r { ctx with no_capture = true } state k
      | Nest r ->
        (* [nest] resets the captures made inside [r] on every entry, so only the
           last entry's captures survive. *)
        let matches =
          List.fold_left (group_names r) ~init:state.matches ~f:(fun matches name ->
            String_map.remove name matches)
        in
        reference r ctx { state with matches } k
      | Case r -> reference r { ctx with case_insens = false } state k
      | No_case r -> reference r { ctx with case_insens = true } state k
      | Intersection _ | Complement _ | Difference _ ->
        (match consume_byte ctx state.pos with
         | Some (c, pos') when view_matches_byte ctx r c -> k { state with pos = pos' }
         | _ -> ())
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
         { str; start; stop; rep = `Greedy; case_insens = false; no_capture = false }
         { pos; matches = String_map.empty })
    |> Option.map (fun state -> state.matches)
  ;;

  let all_offset comp matches =
    Option.map
      (fun m ->
         let offsets = Array.make (Re.group_count comp) (-1, -1) in
         (match String_map.find_opt (group_name 0) m with
          | Some p -> offsets.(0) <- p
          | None -> ());
         List.iter (Re.group_names comp) ~f:(fun (name, idx) ->
           match String_map.find_opt name m with
           | Some p -> offsets.(idx) <- p
           | None -> ());
         offsets)
      matches
  ;;

  let same_execution (_group_counter, re) ?pos ?len input =
    let comp = Re.compile re in
    let res1 = exec_opt re input ?pos ?len in
    let res1_list = all_offset comp res1 in
    let res2 = Re.exec_opt comp input ?pos ?len in
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
    C.add_test
      ~name:"compare_to_reference_window"
      [ re_gen; string_gen_dyn 6; C.range 7; C.range 7 ]
      (fun re input pos len ->
         let pos = pos mod (String.length input + 1) in
         let len = len mod (String.length input - pos + 1) in
         same_execution re input ~pos ~len);
    ()
  ;;

  let () =
    if false
    then (
      let group_counter = ref 0 in
      let manual_test re ?pos ?len str =
        let comp = Re.compile re in
        let res1 = exec_opt re ?pos ?len str in
        let res1_list = all_offset comp res1 in
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
    (* [re_gen_partial] avoids two constructs with known partial-match bugs
       until they are fixed: bounded repetition together with [stop]/[eos] can
       produce a non-conservative `Partial position (#718), and [leol] is
       reported as `Full before a final newline (#717, fix in #721). Use
       [re_gen] once both are fixed. *)
    C.add_test
      ~name:"exec_partial"
      [ re_gen_partial; string_gen_dyn 6; string_gen_dyn 6 ]
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
  Exec_partial.add_test ()
;;

(* This fuzzing runs in two modes:

   1. A bounded seeded quickcheck pass is wired into `dune runtest` (see dune).
   2. For deeper searches, run the exe directly with a larger `--repeat`, or under
      afl-fuzz:

      {v
      mkdir -p _build/input
      AFL_SKIP_CPUFREQ=1 afl-fuzz -i _build/input -o _build/output \
        _build/default/lib_test/fuzz/fuzz.exe @@
      v}

   The fixed seed keeps CI deterministic; bump it when adding generators. *)
