(*
   RE - A regular expression library

   Copyright (C) 2001 Jerome Vouillon
   email: Jerome.Vouillon@pps.jussieu.fr

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation, with
   linking exception; either version 2.1 of the License, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*)

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(****)

let unknown = -2
let break = -3

type match_info =
  | Match of Group.t
  | Failed
  | Running of { no_match_starts_before : int }

type state_info =
  { idx : int
  ; (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    real_idx : int
  ; (* The real index, in case [idx] is set to [break] *)
    mutable final : (Category.t * (Automata.idx * Automata.status)) list
  ; (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t (* Description of this state of the automata *)
  }

(* A state [t] is a pair composed of some information about the
   state [state_info] and a transition table [t array], indexed by
   color. For performance reason, to avoid an indirection, we manually
   unbox the transition table: we allocate a single array, with the
   state information at index 0, followed by the transitions. *)
module State : sig
  type t

  val make : ncol:int -> state_info -> t
  val get_info : t -> state_info
  val follow_transition : t -> color:Cset.c -> t
  val set_transition : t -> color:Cset.c -> t -> unit
end = struct
  type t = Table of t array [@@unboxed]

  let get_info (Table st) : state_info = Obj.magic (Array.unsafe_get st 0)
  [@@inline always]
  ;;

  let set_info (Table st) (info : state_info) = st.(0) <- Obj.magic info

  let follow_transition (Table st) ~color = Array.unsafe_get st (1 + Cset.to_int color)
  [@@inline always]
  ;;

  let set_transition (Table st) ~color st' = st.(1 + Cset.to_int color) <- st'
  let dummy (info : state_info) = Table [| Obj.magic info |]

  let unknown_state =
    dummy { idx = unknown; real_idx = 0; final = []; desc = Automata.State.dummy }
  ;;

  let make ~ncol state =
    let st = Table (Array.make (ncol + 1) unknown_state) in
    set_info st state;
    st
  ;;
end

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr
  ; (* The whole regular expression *)
    mutable initial_states : (Category.t * State.t) list
  ; (* Initial states, indexed by initial category *)
    colors : string
  ; (* Color table *)
    color_repr : string
  ; (* Table from colors to one character of this color *)
    ncolor : int
  ; (* Number of colors. *)
    lnl : int
  ; (* Color of the last newline. -1 if unnecessary *)
    tbl : Automata.Working_area.t
  ; (* Temporary table used to compute the first available index
       when computing a new state *)
    states : State.t Automata.State.Table.t
  ; (* States of the deterministic automata *)
    group_names : (string * int) list
  ; (* Named groups in the regular expression *)
    group_count : int (* Number of groups in the regular expression *)
  }

let pp_re ch re = Automata.pp ch re.initial
let print_re = pp_re
let group_count re = re.group_count
let group_names re = re.group_names

(* Information used during matching *)
type info =
  { re : re
  ; (* The automata *)
    mutable positions : int array
  ; (* Array of mark positions
       The mark are off by one for performance reasons *)
    pos : int
  ; (* Position where the match is started *)
    last : int (* Position where the match should stop *)
  }

(****)

let category re ~color =
  let color = Cset.to_int color in
  if color = -1
  then Category.inexistant (* Special category for the last newline *)
  else if color = re.lnl
  then Category.(lastnewline ++ newline ++ not_letter)
  else Category.from_char re.color_repr.[color]
;;

(****)

let mk_state ncol desc =
  let break_state =
    match Automata.State.status desc with
    | Automata.Running -> false
    | Automata.Failed | Automata.Match _ -> true
  in
  let st =
    let real_idx = Automata.State.idx desc in
    { idx = (if break_state then break else real_idx); real_idx; final = []; desc }
  in
  State.make ~ncol:(if break_state then 0 else ncol) st
;;

let find_state re desc =
  try Automata.State.Table.find re.states desc with
  | Not_found ->
    let st = mk_state re.ncolor desc in
    Automata.State.Table.add re.states desc st;
    st
;;

(**** Match with marks ****)

let delta info cat ~color st =
  let desc = Automata.delta info.re.tbl cat color st.desc in
  let len = Array.length info.positions in
  if Automata.State.idx desc = len && len > 0
  then (
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len);
  desc
;;

let validate info (s : string) ~pos st =
  let color = Cset.of_int @@ Char.code info.re.colors.[Char.code s.[pos]] in
  let st' =
    let desc' =
      let cat = category info.re ~color in
      delta info cat ~color (State.get_info st)
    in
    find_state info.re desc'
  in
  State.set_transition st ~color st'
;;

let next colors st s pos =
  let c = Char.code (String.unsafe_get s pos) in
  State.follow_transition st ~color:(Cset.of_int @@ Char.code (String.unsafe_get colors c))
;;

let rec loop info ~colors ~positions s ~pos ~last st0 st =
  if pos < last
  then (
    let st' = next colors st s pos in
    let state_info = State.get_info st' in
    let idx = state_info.idx in
    if idx >= 0
    then (
      Array.unsafe_set positions idx pos;
      loop info ~colors ~positions s ~pos:(pos + 1) ~last st' st')
    else if idx = break
    then (
      Array.unsafe_set positions state_info.real_idx pos;
      st')
    else (
      (* Unknown *)
      validate info s ~pos st0;
      loop info ~colors ~positions:info.positions s ~pos ~last st0 st0))
  else st
;;

let rec loop_no_mark info ~colors s ~pos ~last st0 st =
  if pos < last
  then (
    let st' = next colors st s pos in
    let state_info = State.get_info st' in
    let idx = state_info.idx in
    if idx >= 0
    then loop_no_mark info ~colors s ~pos:(pos + 1) ~last st' st'
    else if idx = break
    then st'
    else (
      (* Unknown *)
      validate info s ~pos st0;
      loop_no_mark info ~colors s ~pos ~last st0 st0))
  else st
;;

let final info st cat =
  try List.assq cat st.final with
  | Not_found ->
    let st' = delta info cat ~color:(Cset.of_int (-1)) st in
    let res = Automata.State.idx st', Automata.State.status st' in
    st.final <- (cat, res) :: st.final;
    res
;;

let find_initial_state re cat =
  try List.assq cat re.initial_states with
  | Not_found ->
    let st = find_state re (Automata.State.create cat re.initial) in
    re.initial_states <- (cat, st) :: re.initial_states;
    st
;;

let get_color re (s : string) pos =
  Cset.of_int
  @@
  if pos < 0
  then -1
  else (
    let slen = String.length s in
    if pos >= slen
    then -1
    else if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n'
    then (* Special case for the last newline *)
      re.lnl
    else Char.code re.colors.[Char.code s.[pos]])
;;

let rec handle_last_newline info ~pos st ~groups =
  let st' = State.follow_transition st ~color:(Cset.of_int info.re.lnl) in
  let info' = State.get_info st' in
  if info'.idx >= 0
  then (
    if groups then info.positions.(info'.idx) <- pos;
    st')
  else if info'.idx = break
  then (
    if groups then info.positions.(info'.real_idx) <- pos;
    st')
  else (
    (* Unknown *)
    let color = Cset.of_int info.re.lnl in
    let st' =
      let desc' =
        let cat = category info.re ~color in
        let real_c = Cset.of_int @@ Char.code info.re.colors.[Char.code '\n'] in
        delta info cat ~color:real_c (State.get_info st)
      in
      find_state info.re desc'
    in
    State.set_transition st ~color st';
    handle_last_newline info ~pos st ~groups)
;;

let rec scan_str info (s : string) initial_state ~groups =
  let pos = info.pos in
  let last = info.last in
  if last = String.length s
     && info.re.lnl <> -1
     && last > pos
     && String.get s (last - 1) = '\n'
  then (
    let info = { info with last = last - 1 } in
    let st = scan_str info s initial_state ~groups in
    if (State.get_info st).idx = break
    then st
    else handle_last_newline info ~pos:(last - 1) st ~groups)
  else if groups
  then
    loop
      info
      ~colors:info.re.colors
      ~positions:info.positions
      s
      ~pos
      ~last
      initial_state
      initial_state
  else loop_no_mark info ~colors:info.re.colors s ~pos ~last initial_state initial_state
;;

(* This function adds a final boundary check on the input.
   This is useful to indicate that the output failed because
   of insufficient input, or to verify that the output actually
   matches for regex that have boundary conditions with respect
   to the input string.
*)
let final_boundary_check ~last ~slen re s ~info ~st ~groups =
  let idx, res =
    let final_cat =
      Category.(
        search_boundary
        ++ if last = slen then inexistant else category re ~color:(get_color re s last))
    in
    final info (State.get_info st) final_cat
  in
  (match groups, res with
   | true, Match _ -> info.positions.(idx) <- last
   | _ -> ());
  res
;;

let match_str ~groups ~partial re s ~pos ~len =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re
    ; pos
    ; last
    ; positions =
        (if groups
         then (
           let n = Automata.Working_area.index_count re.tbl + 1 in
           if n <= 10 then [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |] else Array.make n 0)
         else [||])
    }
  in
  let st =
    let initial_state =
      let initial_cat =
        Category.(
          search_boundary
          ++ if pos = 0 then inexistant else category re ~color:(get_color re s (pos - 1)))
      in
      find_initial_state re initial_cat
    in
    scan_str info s initial_state ~groups
  in
  match
    let state_info = State.get_info st in
    if state_info.idx = break || (partial && not groups)
    then Automata.State.status state_info.desc
    else if partial && groups
    then (
      match Automata.State.status state_info.desc with
      | (Match _ | Failed) as status -> status
      | Running ->
        (* This could be because it's still not fully matched, or it
           could be that because we need to run special end of input
           checks. *)
        (match final_boundary_check ~last ~slen re s ~info ~st ~groups with
         | Match _ as status -> status
         | Failed | Running ->
           (* A failure here just means that we need more data, i.e.
              it's a partial match. *)
           Running))
    else final_boundary_check ~last ~slen re s ~info ~st ~groups
  with
  | Match (marks, pmarks) ->
    Match { s; marks; pmarks; gpos = info.positions; gcount = re.group_count }
  | Failed -> Failed
  | Running ->
    let no_match_starts_before = if groups then info.positions.(0) else 0 in
    Running { no_match_starts_before }
;;

let mk_re ~initial ~colors ~color_repr ~ncolor ~lnl ~group_names ~group_count =
  { initial
  ; initial_states = []
  ; colors
  ; color_repr
  ; ncolor
  ; lnl
  ; tbl = Automata.Working_area.create ()
  ; states = Automata.State.Table.create 97
  ; group_names
  ; group_count
  }
;;

open Ast

(**** Compilation ****)

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
  | `First, `First -> cr
  | `First, k -> A.seq ids k cr (A.eps ids)
  | _ -> cr
;;

(* XXX should probably compute a category mask *)
let rec translate ids kind ~ign_group greedy pos names cache c = function
  | Set s -> A.cst ids (trans_set cache c s), kind
  | Sequence l -> trans_seq ids kind ~ign_group greedy pos names cache c l, kind
  | Alternative l ->
    (match merge_sequences l with
     | [ r' ] ->
       let cr, kind' = translate ids kind ~ign_group greedy pos names cache c r' in
       enforce_kind ids kind kind' cr, kind
     | merged_sequences ->
       ( A.alt
           ids
           (List.map
              (fun r' ->
                let cr, kind' =
                  translate ids kind ~ign_group greedy pos names cache c r'
                in
                enforce_kind ids kind kind' cr)
              merged_sequences)
       , kind ))
  | Repeat (r', i, j) ->
    let cr, kind' = translate ids kind ~ign_group greedy pos names cache c r' in
    let rem =
      match j with
      | None -> A.rep ids greedy kind' cr
      | Some j ->
        let f =
          match greedy with
          | `Greedy ->
            fun rem -> A.alt ids [ A.seq ids kind' (A.rename ids cr) rem; A.eps ids ]
          | `Non_greedy ->
            fun rem -> A.alt ids [ A.eps ids; A.seq ids kind' (A.rename ids cr) rem ]
        in
        iter (j - i) f (A.eps ids)
    in
    iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind
  | Beg_of_line -> A.after ids Category.(inexistant ++ newline), kind
  | End_of_line -> A.before ids Category.(inexistant ++ newline), kind
  | Beg_of_word ->
    ( A.seq
        ids
        `First
        (A.after ids Category.(inexistant ++ not_letter))
        (A.before ids Category.letter)
    , kind )
  | End_of_word ->
    ( A.seq
        ids
        `First
        (A.after ids Category.letter)
        (A.before ids Category.(inexistant ++ not_letter))
    , kind )
  | Not_bound ->
    ( A.alt
        ids
        [ A.seq ids `First (A.after ids Category.letter) (A.before ids Category.letter)
        ; (let cat = Category.(inexistant ++ not_letter) in
           A.seq ids `First (A.after ids cat) (A.before ids cat))
        ]
    , kind )
  | Beg_of_str -> A.after ids Category.inexistant, kind
  | End_of_str -> A.before ids Category.inexistant, kind
  | Last_end_of_line -> A.before ids Category.(inexistant ++ lastnewline), kind
  | Start -> A.after ids Category.search_boundary, kind
  | Stop -> A.before ids Category.search_boundary, kind
  | Sem (kind', r') ->
    let cr, kind'' = translate ids kind' ~ign_group greedy pos names cache c r' in
    enforce_kind ids kind' kind'' cr, kind'
  | Sem_greedy (greedy', r') -> translate ids kind ~ign_group greedy' pos names cache c r'
  | Group (n, r') ->
    if ign_group
    then translate ids kind ~ign_group greedy pos names cache c r'
    else (
      let p = !pos in
      let () =
        match n with
        | Some name -> names := (name, A.Mark.group_count p) :: !names
        | None -> ()
      in
      pos := A.Mark.next2 !pos;
      let cr, kind' = translate ids kind ~ign_group greedy pos names cache c r' in
      ( A.seq ids `First (A.mark ids p) (A.seq ids `First cr (A.mark ids (A.Mark.next p)))
      , kind' ))
  | No_group r' -> translate ids kind ~ign_group:true greedy pos names cache c r'
  | Nest r' ->
    let b = !pos in
    let cr, kind' = translate ids kind ~ign_group greedy pos names cache c r' in
    let e = A.Mark.prev !pos in
    if e < b then cr, kind' else A.seq ids `First (A.erase ids b e) cr, kind'
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ -> assert false
  | Pmark (i, r') ->
    let cr, kind' = translate ids kind ~ign_group greedy pos names cache c r' in
    A.seq ids `First (A.pmark ids i) cr, kind'

and trans_seq ids kind ~ign_group greedy pos names cache c = function
  | [] -> A.eps ids
  | [ r ] ->
    let cr', kind' = translate ids kind ~ign_group greedy pos names cache c r in
    enforce_kind ids kind kind' cr'
  | r :: rem ->
    let cr', kind' = translate ids kind ~ign_group greedy pos names cache c r in
    let cr'' = trans_seq ids kind ~ign_group greedy pos names cache c rem in
    if A.is_eps cr'' then cr' else if A.is_eps cr' then cr'' else A.seq ids kind' cr' cr''
;;

let compile_1 regexp =
  let regexp = handle_case false regexp in
  let c = Color_map.make () in
  let need_lnl = colorize c regexp in
  let colors, color_repr, ncolor = Color_map.flatten c in
  let lnl = if need_lnl then ncolor else -1 in
  let ncolor = if need_lnl then ncolor + 1 else ncolor in
  let ids = A.Ids.create () in
  let pos = ref A.Mark.start in
  let names = ref [] in
  let r, kind =
    translate
      ids
      `First
      ~ign_group:false
      `Greedy
      pos
      names
      (ref Cset.CSetMap.empty)
      colors
      regexp
  in
  let r = enforce_kind ids `First kind r in
  (*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re
    ~initial:r
    ~colors
    ~color_repr
    ~ncolor
    ~lnl
    ~group_names:(List.rev !names)
    ~group_count:(A.Mark.group_count !pos)
;;

(****)

let compile r =
  compile_1 (if anchored r then group r else seq [ shortest (rep any); group r ])
;;

let exec_internal name ?(pos = 0) ?(len = -1) ~partial ~groups re s =
  if pos < 0 || len < -1 || pos + len > String.length s then invalid_arg name;
  match_str ~groups ~partial re s ~pos ~len
;;

let exec ?pos ?len re s =
  match exec_internal "Re.exec" ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> substr
  | _ -> raise Not_found
;;

let exec_opt ?pos ?len re s =
  match exec_internal "Re.exec_opt" ?pos ?len ~groups:true ~partial:false re s with
  | Match substr -> Some substr
  | _ -> None
;;

let execp ?pos ?len re s =
  match exec_internal ~groups:false ~partial:false "Re.execp" ?pos ?len re s with
  | Match _substr -> true
  | _ -> false
;;

let exec_partial ?pos ?len re s =
  match exec_internal ~groups:false ~partial:true "Re.exec_partial" ?pos ?len re s with
  | Match _ -> `Full
  | Running _ -> `Partial
  | Failed -> `Mismatch
;;

let exec_partial_detailed ?pos ?len re s =
  match
    exec_internal ~groups:true ~partial:true "Re.exec_partial_detailed" ?pos ?len re s
  with
  | Match group -> `Full group
  | Running { no_match_starts_before } -> `Partial no_match_starts_before
  | Failed -> `Mismatch
;;

module Mark = struct
  type t = Pmark.t

  let test (g : Group.t) p = Pmark.Set.mem p g.pmarks
  let all (g : Group.t) = g.pmarks

  module Set = Pmark.Set

  let equal = Pmark.equal
  let compare = Pmark.compare
end

type split_token =
  [ `Text of string
  | `Delim of Group.t
  ]

module Rseq = struct
  let all ?(pos = 0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.all";
    (* index of the first position we do not consider.
       !pos < limit is an invariant *)
    let limit =
      match len with
      | None -> String.length s
      | Some l ->
        if l < 0 || pos + l > String.length s then invalid_arg "Re.all";
        pos + l
    in
    (* iterate on matches. When a match is found, search for the next
       one just after its end *)
    let rec aux pos on_match () =
      if pos > limit
      then Seq.Nil (* no more matches *)
      else (
        match match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos) with
        | Match substr ->
          let p1, p2 = Group.offset substr 0 in
          if on_match && p1 = pos && p1 = p2
          then (* skip empty match right after a match *)
            aux (pos + 1) false ()
          else (
            let pos = if p1 = p2 then p2 + 1 else p2 in
            Seq.Cons (substr, aux pos (p1 <> p2)))
        | Running _ | Failed -> Seq.Nil)
    in
    aux pos false
  ;;

  let matches ?pos ?len re s : _ Seq.t =
    all ?pos ?len re s |> Seq.map (fun sub -> Group.get sub 0)
  ;;

  let split_full ?(pos = 0) ?len re s : _ Seq.t =
    if pos < 0 then invalid_arg "Re.split";
    let limit =
      match len with
      | None -> String.length s
      | Some l ->
        if l < 0 || pos + l > String.length s then invalid_arg "Re.split";
        pos + l
    in
    (* i: start of delimited string
       pos: first position after last match of [re]
       limit: first index we ignore (!pos < limit is an invariant) *)
    let pos0 = pos in
    let rec aux state i pos () =
      match state with
      | `Idle when pos > limit ->
        (* We had an empty match at the end of the string *)
        assert (i = limit);
        Seq.Nil
      | `Idle ->
        (match match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos) with
         | Match substr ->
           let p1, p2 = Group.offset substr 0 in
           let pos = if p1 = p2 then p2 + 1 else p2 in
           let old_i = i in
           let i = p2 in
           if old_i = p1 && p1 = p2 && p1 > pos0
           then (* Skip empty match right after a delimiter *)
             aux state i pos ()
           else if p1 > pos0
           then (
             (* string does not start by a delimiter *)
             let text = String.sub s old_i (p1 - old_i) in
             let state = `Yield (`Delim substr) in
             Seq.Cons (`Text text, aux state i pos))
           else Seq.Cons (`Delim substr, aux state i pos)
         | Running _ -> Seq.Nil
         | Failed ->
           if i < limit
           then (
             let text = String.sub s i (limit - i) in
             (* yield last string *)
             Seq.Cons (`Text text, aux state limit pos))
           else Seq.Nil)
      | `Yield x -> Seq.Cons (x, aux `Idle i pos)
    in
    aux `Idle pos pos
  ;;

  let split ?pos ?len re s : _ Seq.t =
    let seq = split_full ?pos ?len re s in
    let rec filter seq () =
      match seq () with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (`Delim _, tl) -> filter tl ()
      | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter tl)
    in
    filter seq
  ;;

  let split_delim ?pos ?len re s : _ Seq.t =
    let seq = split_full ?pos ?len re s in
    let rec filter ~delim seq () =
      match seq () with
      | Seq.Nil -> if delim then Seq.Cons ("", fun () -> Seq.Nil) else Seq.Nil
      | Seq.Cons (`Delim _, tl) ->
        if delim
        then Seq.Cons ("", fun () -> filter ~delim:true tl ())
        else filter ~delim:true tl ()
      | Seq.Cons (`Text s, tl) -> Seq.Cons (s, filter ~delim:false tl)
    in
    filter ~delim:true seq
  ;;
end

module Rlist = struct
  let list_of_seq (s : 'a Seq.t) : 'a list =
    Seq.fold_left (fun l x -> x :: l) [] s |> List.rev
  ;;

  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> list_of_seq
  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> list_of_seq
  let split_full ?pos ?len re s = Rseq.split_full ?pos ?len re s |> list_of_seq
  let split ?pos ?len re s = Rseq.split ?pos ?len re s |> list_of_seq
  let split_delim ?pos ?len re s = Rseq.split_delim ?pos ?len re s |> list_of_seq
end

module Gen = struct
  type 'a gen = unit -> 'a option

  let gen_of_seq (s : 'a Seq.t) : 'a gen =
    let r = ref s in
    fun () ->
      match !r () with
      | Seq.Nil -> None
      | Seq.Cons (x, tl) ->
        r := tl;
        Some x
  ;;

  let split ?pos ?len re s : _ gen = Rseq.split ?pos ?len re s |> gen_of_seq
  let split_full ?pos ?len re s : _ gen = Rseq.split_full ?pos ?len re s |> gen_of_seq
  let all ?pos ?len re s = Rseq.all ?pos ?len re s |> gen_of_seq
  let matches ?pos ?len re s = Rseq.matches ?pos ?len re s |> gen_of_seq
end

let replace ?(pos = 0) ?len ?(all = true) re ~f s =
  if pos < 0 then invalid_arg "Re.replace";
  let limit =
    match len with
    | None -> String.length s
    | Some l ->
      if l < 0 || pos + l > String.length s then invalid_arg "Re.replace";
      pos + l
  in
  (* buffer into which we write the result *)
  let buf = Buffer.create (String.length s) in
  (* iterate on matched substrings. *)
  let rec iter pos on_match =
    if pos <= limit
    then (
      match match_str ~groups:true ~partial:false re s ~pos ~len:(limit - pos) with
      | Match substr ->
        let p1, p2 = Group.offset substr 0 in
        if pos = p1 && p1 = p2 && on_match
        then (
          (* if we matched an empty string right after a match,
             we must manually advance by 1 *)
          if p2 < limit then Buffer.add_char buf s.[p2];
          iter (p2 + 1) false)
        else (
          (* add string between previous match and current match *)
          Buffer.add_substring buf s pos (p1 - pos);
          (* what should we replace the matched group with? *)
          let replacing = f substr in
          Buffer.add_string buf replacing;
          if all
          then
            (* if we matched an empty string, we must manually advance by 1 *)
            iter
              (if p1 = p2
               then (
                 (* a non char could be past the end of string. e.g. $ *)
                 if p2 < limit then Buffer.add_char buf s.[p2];
                 p2 + 1)
               else p2)
              (p1 <> p2)
          else Buffer.add_substring buf s p2 (limit - p2))
      | Running _ -> ()
      | Failed -> Buffer.add_substring buf s pos (limit - pos))
  in
  iter pos false;
  Buffer.contents buf
;;

let replace_string ?pos ?len ?all re ~by s = replace ?pos ?len ?all re s ~f:(fun _ -> by)

let witness t =
  let rec witness = function
    | Set c -> String.make 1 (Char.chr (Cset.pick c |> Cset.to_int))
    | Sequence xs -> String.concat "" (List.map witness xs)
    | Alternative (x :: _) -> witness x
    | Alternative [] -> assert false
    | Repeat (r, from, _to) ->
      let w = witness r in
      let b = Buffer.create (String.length w * from) in
      for _i = 1 to from do
        Buffer.add_string b w
      done;
      Buffer.contents b
    | No_case r -> witness r
    | Intersection _ | Complement _ | Difference (_, _) -> assert false
    | Group (_, r)
    | No_group r
    | Nest r
    | Sem (_, r)
    | Pmark (_, r)
    | Case r
    | Sem_greedy (_, r) -> witness r
    | Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | Last_end_of_line
    | Start
    | Stop
    | End_of_str -> ""
  in
  witness (handle_case false t)
;;

module Seq = Rseq
module List = Rlist
module Group = Group

(** {2 Deprecated functions} *)

let split_full_seq = Seq.split_full
let split_seq = Seq.split
let matches_seq = Seq.matches
let all_seq = Seq.all

type 'a gen = 'a Gen.gen

let all_gen = Gen.all
let matches_gen = Gen.matches
let split_gen = Gen.split
let split_full_gen = Gen.split_full

type substrings = Group.t

let get = Group.get
let get_ofs = Group.offset
let get_all = Group.all
let get_all_ofs = Group.all_offset
let test = Group.test

type markid = Mark.t

let marked = Mark.test
let mark_set = Mark.all

(**********************************)

(*
   Information about the previous character:
   - does not exists
   - is a letter
   - is not a letter
   - is a newline
   - is last newline

   Beginning of word:
   - previous is not a letter or does not exist
   - current is a letter or does not exist

   End of word:
   - previous is a letter or does not exist
   - current is not a letter or does not exist

   Beginning of line:
   - previous is a newline or does not exist

   Beginning of buffer:
   - previous does not exist

   End of buffer
   - current does not exist

   End of line
   - current is a newline or does not exist
*)

(*
   Rep: e = T,e | ()
  - semantics of the comma (shortest/longest/first)
  - semantics of the union (greedy/non-greedy)

Bounded repetition
  a{0,3} = (a,(a,a?)?)?
*)

type groups = Group.t

include Rlist
include Ast

module View = struct
  include Ast

  let view = Fun.id
end

let pp = Ast.pp
