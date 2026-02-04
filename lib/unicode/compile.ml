open Import

module type T = sig
  type re
  type ast

  module Stream : sig
    type t
    type 'a feed = Ok of 'a | No_match

    val create : re -> t
    val feed : t -> string -> pos:int -> len:int -> t feed
    val finalize : t -> string -> pos:int -> len:int -> bool

    module Group : sig
      type stream := t
      type t

      module Match : sig
        type t

        val get : t -> int -> string option
        val test_mark : t -> Pmark.t -> bool
      end

      val create : stream -> t
      val feed : t -> string -> pos:int -> len:int -> t feed
      val finalize : t -> string -> pos:int -> len:int -> Match.t feed
      val no_match_starts_before : t -> int
    end
  end

  type match_info =
    | Match of Group.t
    | Failed
    | Running of { no_match_starts_before : int }

  val match_str_no_bounds :
    groups:bool ->
    partial:bool ->
    re ->
    string ->
    pos:int ->
    len:int ->
    match_info

  val match_str :
    groups:bool ->
    partial:bool ->
    re ->
    string ->
    pos:int ->
    len:int ->
    match_info

  val match_str_p : re -> string -> pos:int -> len:int -> bool
  val compile : ast -> re
  val group_count : re -> int
  val group_names : re -> (string * int) list
  val pp_re : re Fmt.t
end

let rec iter n f v = if Int.equal n 0 then v else iter (n - 1) f (f v)

module Idx : sig
  type t [@@immediate]

  val unknown : t
  val make_break : Automata.Idx.t -> t
  val of_idx : Automata.Idx.t -> t
  val is_idx : t -> bool
  val is_break : t -> bool
  val is_unknown : t -> bool
  val idx : t -> int
  val break_idx : t -> int
end = struct
  type t = int

  let unknown = -2
  let break = -3
  let of_idx (x : Automata.Idx.t) = Automata.Idx.to_int x [@@inline always]
  let is_idx t = t >= 0 [@@inline always]
  let is_break x = x <= break [@@inline always]
  let is_unknown x = x = unknown [@@inline always]
  let idx t = t [@@inline always]

  let make_break (idx : Automata.Idx.t) = -5 - Automata.Idx.to_int idx
  [@@inline always]

  let break_idx t = (t + 5) * -1 [@@inline always]
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module Ast = struct
    include Ast
    include Ast.Make (Cset) (Color_map)
  end

  module Automata = struct
    include Automata
    include Automata.Make (Cset)
  end

  module Category = struct
    include Category
    include Category.Make (Cset)
  end

  type ast = Ast.t

  type state_info = {
    idx : Idx.t;
    (* Index of the current position in the position table.
       Not yet computed transitions point to a dummy state where
       [idx] is set to [unknown];
       If [idx] is set to [break] for states that either always
       succeed or always fail. *)
    mutable final : (Category.t * (Automata.Idx.t * Automata.Status.t)) list;
    (* Mapping from the category of the next character to
       - the index where the next position should be saved
       - possibly, the list of marks (and the corresponding indices)
         corresponding to the best match *)
    desc : Automata.State.t; (* Description of this state of the automata *)
  }

  (* Thread-safety: we use double-checked locking to access field [final]. *)

  (* A state [t] is a pair composed of some information about the
   state [state_info] and a transition table [t array], indexed by
   color. For performance reason, to avoid an indirection, we manually
   unbox the transition table: we allocate a single array, with the
   state information at index 0, followed by the transitions. *)
  module State : sig
    type t

    val make : ncol:int -> state_info -> t
    val make_break : state_info -> t
    val get_info : t -> state_info
    val follow_transition : t -> color:Cset.cp -> t
    val set_transition : t -> color:Cset.cp -> t -> unit
    val is_unknown_transition : t -> color:Cset.cp -> bool
  end = struct
    type t = Table of t array [@@unboxed]

    (* Thread-safety:
     We store the state information at index 0. For other elements
     of the transition table, which are lazily computed, we use
     double-checked locking. *)

    let get_info (Table st) : state_info = Obj.magic (Array.unsafe_get st 0)
    [@@inline always]

    let set_info (Table st) (info : state_info) = st.(0) <- Obj.magic info

    let follow_transition (Table st) ~color =
      Array.unsafe_get st (1 + Cset.CodePage.to_int color)
    [@@inline always]

    let set_transition (Table st) ~color st' =
      st.(1 + Cset.CodePage.to_int color) <- st'

    let is_unknown_transition st ~color =
      let st' = follow_transition st ~color in
      let info = get_info st' in
      Idx.is_unknown info.idx

    let dummy (info : state_info) = Table [| Obj.magic info |]

    let unknown_state =
      dummy { idx = Idx.unknown; final = []; desc = Automata.State.dummy }

    let make ~ncol state =
      let st = Table (Array.make (ncol + 1) unknown_state) in
      set_info st state;
      st

    let make_break state = Table [| Obj.magic state |]
  end

  (* Automata (compiled regular expression) *)
  type re = {
    initial : Automata.expr;
    (* The whole regular expression *)
    mutable initial_states : (Category.t * State.t) list;
    (* Initial states, indexed by initial category *)
    colors : Color_map.Table.t;
    (* Color table *)
    color_repr : Color_map.Repr.t;
    (* Table from colors to one character of this color *)
    ncolor : int;
    (* Number of colors. *)
    lnl : Cset.cp;
    (* Color of the last newline. [Cset.null_char] if unnecessary *)
    tbl : Automata.Working_area.t;
    (* Temporary table used to compute the first available index
       when computing a new state *)
    states : State.t Automata.State.Table.t;
    (* States of the deterministic automata *)
    group_names : (string * int) list;
    (* Named groups in the regular expression *)
    group_count : int;
    (* Number of groups in the regular expression *)
    mutex : Mutex.t;
  }

  (* Thread-safety:
   We use double-checked locking to access field [initial_states]. The
   state table [states] and the working area [tbl] are only accessed
   with the mutex [mutex] locked.
   The working area is shared between all threads. This might be
   inefficient if many threads are updating the automaton. It seems
   complicated to manage a working area per domain and per regular
   expression. So, if this becomes an issue, it might just be simpler
   to allocate a fresh working area whenever needed.
*)

  let pp_re ch re = Automata.pp ch re.initial
  let group_count re = re.group_count
  let group_names re = re.group_names

  module Positions = struct
    (* Information used during matching *)
    type t = {
      mutable positions : int array;
      (* Array of mark positions
         The mark are off by one for performance reasons *)
      mutable length : int;
    }

    let empty = { positions = [||]; length = 0 }
    let length t = t.length
    let unsafe_set t idx pos = Array.unsafe_set t.positions idx pos

    let rec resize idx t =
      t.length <- 2 * t.length;
      if idx >= t.length then resize idx t
      else
        let pos = t.positions in
        t.positions <- Array.make t.length 0;
        Array.blit pos 0 t.positions 0 (Array.length pos)

    let set t idx pos =
      if idx >= length t then resize idx t;
      unsafe_set t idx pos

    let all t = t.positions
    let first t = t.positions.(0)

    let make ~groups re =
      if groups then
        (* We initialize this table with a reasonable size. The required
         size may change when the automaton gets updated. So we are
         always checking whether it is large enough before modifying it. *)
        let length = Automata.Working_area.index_count re.tbl + 1 in
        { positions = Array.make length 0; length }
      else empty
  end

  (****)

  let category re ~color =
    if Cset.CodePage.equal color Cset.CodePage.null then Category.inexistant
      (* Special category for the last newline *)
    else if Cset.CodePage.equal color re.lnl then
      Category.(lastnewline ++ newline ++ not_letter)
    else
      Category.from_letter
      @@ Color_map.Repr.repr re.color_repr (Cset.CodePage.to_int color)

  (****)

  let find_state re desc =
    try Automata.State.Table.find re.states desc
    with Not_found ->
      let st =
        let break_state =
          match Automata.State.status_no_mutex desc with
          | Running -> false
          | Failed | Match _ -> true
        in
        let st =
          {
            idx =
              (let idx = Automata.State.idx desc in
               if break_state then Idx.make_break idx else Idx.of_idx idx);
            final = [];
            desc;
          }
        in
        if break_state then State.make_break st
        else State.make ~ncol:re.ncolor st
      in
      st

  (**** Match with marks ****)

  let delta re cat ~color st =
    Automata.delta re.tbl cat color st.desc

  let validate re (s : string) ~pos st =
    let letter =
      Cset.Codec.Unsafe.unsafe_bytes s pos |> Cset.Codec.from_bytes
    in
    let color = Color_map.Table.get re.colors letter in
    Mutex.lock re.mutex;
    (if State.is_unknown_transition st ~color then
       let st' =
         let desc' =
           let cat = category re ~color in
           delta re cat ~color (State.get_info st)
         in
         find_state re desc'
       in
       State.set_transition st ~color st');
    Mutex.unlock re.mutex

  let next colors st s pos =
    State.follow_transition st
      ~color:
        (Cset.Codec.Unsafe.unsafe_bytes s pos
        |> Cset.Codec.from_bytes |> Color_map.Table.get colors)

  let rec loop re ~colors ~positions s ~pos ~last st0 st =
    if pos < last then
      let st' = next colors st s pos in
      let idx = (State.get_info st').idx in
      if Idx.is_idx idx then
        if
          Idx.idx idx < Positions.length positions
        then (
          Positions.unsafe_set positions (Idx.idx idx) pos;
          let w = Cset.Codec.width String.unsafe_get s pos in
          loop re ~colors ~positions s ~pos:(pos + w) ~last st' st')
        else (
          Positions.set positions (Idx.idx idx) pos;
          let w = Cset.Codec.width String.unsafe_get s pos in
          loop re ~colors ~positions s ~pos:(pos + w) ~last st' st')
      else if Idx.is_break idx then (
        Positions.set positions (Idx.break_idx idx) pos;
        st')
      else (
        (* Unknown *)
        validate re s ~pos st0;
        loop re ~colors ~positions s ~pos ~last st0 st0)
    else
      st

  let rec loop_no_mark re ~colors s ~pos ~last st0 st =
    if pos < last then
      let st' = next colors st s pos in
      let idx = (State.get_info st').idx in
      if Idx.is_idx idx then
        let w = Cset.Codec.width String.unsafe_get s pos in
        loop_no_mark re ~colors s ~pos:(pos + w) ~last st' st'
      else if Idx.is_break idx then
        st'
      else (
        (* Unknown *)
        validate re s ~pos st0;
        loop_no_mark re ~colors s ~pos ~last st0 st0)
    else st

  let final re st cat =
    try List.assq cat st.final
    with Not_found ->
      Mutex.lock re.mutex;
      let res =
        try List.assq cat st.final
        with Not_found ->
          let st' = delta re cat ~color:Cset.CodePage.null st in
          let res =
            (Automata.State.idx st', Automata.State.status_no_mutex st')
          in
          st.final <- (cat, res) :: st.final;
          res
      in
      Mutex.unlock re.mutex;
      res

  let find_initial_state re cat =
    try List.assq cat re.initial_states
    with Not_found ->
      Mutex.lock re.mutex;
      let res =
        try List.assq cat re.initial_states
        with Not_found ->
          let st = find_state re (Automata.State.create cat re.initial) in
          re.initial_states <- (cat, st) :: re.initial_states;
          st
      in
      Mutex.unlock re.mutex;
      res

  let get_color re (s : string) pos =
    if pos < 0 then Cset.CodePage.null
    else
      let slen = String.length s in
      if pos >= slen then Cset.CodePage.null
      else if
        pos = slen - 1
        && (not (Cset.CodePage.equal re.lnl Cset.CodePage.null))
        && Cset.CodePage.equal
             (Cset.Codec.Unsafe.unsafe_bytes_rev s (slen - 1)
             |> Cset.Codec.from_bytes |> Cset.CodePage.from_letter)
             (Cset.Codec.new_line |> Cset.CodePage.from_letter)
      then
        (* Special case for the last newline *)
        re.lnl
      else
        let letter =
          Cset.Codec.Unsafe.unsafe_bytes s pos |> Cset.Codec.from_bytes
        in
        Color_map.Table.get re.colors letter

  let rec handle_last_newline re positions ~pos st ~groups =
    let st' = State.follow_transition st ~color:re.lnl in
    let info = State.get_info st' in
    if Idx.is_idx info.idx then (
      if groups then Positions.set positions (Idx.idx info.idx) pos;
      st')
    else if Idx.is_break info.idx then (
      if groups then Positions.set positions (Idx.break_idx info.idx) pos;
      st')
    else
      (* Unknown *)
      let color = re.lnl in
      Mutex.lock re.mutex;
      (if State.is_unknown_transition st ~color then
         let st' =
           let desc =
             let cat = category re ~color in
             let real_c =
               Color_map.Table.get re.colors Cset.Codec.new_line
               (* '\n' *)
             in
             delta re cat ~color:real_c (State.get_info st)
           in
           find_state re desc
         in
         State.set_transition st ~color st');
      Mutex.unlock re.mutex;
      handle_last_newline re positions ~pos st ~groups

  let rec scan_str re positions (s : string) initial_state ~last ~pos ~groups =
    if
      last = String.length s
      && (not (Cset.CodePage.equal re.lnl Cset.CodePage.null))
      && last > pos
      &&
        try
          Cset.CodePage.equal
            (Cset.Codec.Unsafe.unsafe_bytes_rev s (last - 1)
            |> Cset.Codec.from_bytes |> Cset.CodePage.from_letter)
            (Cset.Codec.new_line |> Cset.CodePage.from_letter)
        with _ -> false
    then
      let w = Cset.Codec.width_rev String.unsafe_get s (last - 1) in
      let last = last - w in
      let st = scan_str re positions ~pos s initial_state ~last ~groups in
      if Idx.is_break (State.get_info st).idx then st
      else handle_last_newline re positions ~pos:last st ~groups
    else if groups then
      loop re ~colors:re.colors ~positions s ~pos ~last initial_state
        initial_state
    else
      loop_no_mark re ~colors:re.colors s ~pos ~last initial_state initial_state

  (* This function adds a final boundary check on the input.
   This is useful to indicate that the output failed because
   of insufficient input, or to verify that the output actually
   matches for regex that have boundary conditions with respect
   to the input string.
*)
  let final_boundary_check re positions ~last ~slen s state_info ~groups =
    let idx, res =
      let final_cat =
        Category.(
          search_boundary
          ++
          if last = slen then inexistant
          else category re ~color:(get_color re s last))
      in
      final re state_info final_cat
    in
    (match (groups, res) with
    | true, Match _ -> Positions.set positions (Automata.Idx.to_int idx) last
    | _ -> ());
    res

  let make_match_str re positions ~len ~groups ~partial s ~pos =
    let slen = String.length s in
    let last = if len = -1 then slen else pos + len in
    let st =
      let initial_state =
        let initial_cat =
          Category.(
            search_boundary
            ++
            if pos = 0 then inexistant
            else
              let w = Cset.Codec.width_rev String.unsafe_get s (pos - 1) in
              category re ~color:(get_color re s (pos - w)))
        in
        let st = find_initial_state re initial_cat in
        st
      in
      scan_str re positions s initial_state ~pos ~last ~groups
    in
    let state_info = State.get_info st in
    if Idx.is_break state_info.idx || (partial && not groups) then
      Automata.State.status re.mutex state_info.desc
    else if partial && groups then
      match Automata.State.status re.mutex state_info.desc with
      | (Match _ | Failed) as status ->
        status
      | Running -> (
        (* This could be because it's still not fully matched, or it
         could be that because we need to run special end of input
         checks. *)
        match
          final_boundary_check re positions ~last ~slen s state_info ~groups
        with
        | Match _ as status ->
          status
        | Failed | Running ->
          (* A failure here just means that we need more data, i.e.
            it's a partial match. *)
          Running)
    else
      final_boundary_check re positions ~last ~slen s state_info ~groups

  module Stream = struct
    type nonrec t = { state : State.t; re : re }
    type 'a feed = Ok of 'a | No_match

    let create re =
      let category = Category.(search_boundary ++ inexistant) in
      let state = find_initial_state re category in
      { state; re }

    let feed t s ~pos ~len =
      (* TODO bound checks? *)
      let last = pos + len in
      let state =
        loop_no_mark t.re ~colors:t.re.colors s ~last ~pos t.state t.state
      in
      let info = State.get_info state in
      if
        Idx.is_break info.idx
        &&
        match Automata.State.status t.re.mutex info.desc with
        | Failed -> true
        | Match _ | Running -> false
      then No_match
      else Ok { t with state }

    let finalize t s ~pos ~len =
      (* TODO bound checks? *)
      let last = pos + len in
      let state =
        scan_str t.re Positions.empty s t.state ~last ~pos ~groups:false
      in
      let info = State.get_info state in
      match
        let _idx, res =
          let final_cat = Category.(search_boundary ++ inexistant) in
          final t.re info final_cat
        in
        res
      with
      | Running | Failed -> false
      | Match _ -> true

    module Group = struct
      type nonrec t = {
        t : t;
        positions : Positions.t;
        slices : Slice.L.t;
        abs_pos : int;
        first_match_pos : int;
      }

      let no_match_starts_before t = t.first_match_pos

      let create t =
        {
          t;
          positions = Positions.make ~groups:true t.re;
          slices = [];
          abs_pos = 0;
          first_match_pos = 0;
        }

      module Match = struct
        type t = {
          pmarks : Pmark.Set.t;
          slices : Slice.L.t;
          marks : Mark_infos.t;
          positions : int array;
          start_pos : int;
        }

        let test_mark t mark = Pmark.Set.mem mark t.pmarks

        let get t i =
          Mark_infos.offset t.marks i
          |> Option.map (fun (start, stop) ->
            let start = t.positions.(start) - t.start_pos in
            let stop = t.positions.(stop) - t.start_pos in
            Slice.L.get_substring t.slices ~start ~stop)

        let make ~start_pos ~pmarks ~slices ~marks ~positions =
          let positions = Positions.all positions in
          { pmarks; slices; positions; marks; start_pos }
      end

      let rec loop re ~abs_pos ~colors ~positions s ~pos ~last st0 st =
        if pos < last then
          let st' = next colors st s pos in
          let idx = (State.get_info st').idx in
          if Idx.is_idx idx then
            if Idx.idx idx < Positions.length positions then (
              Positions.unsafe_set positions (Idx.idx idx) (abs_pos + pos);
              let w = Cset.Codec.width String.unsafe_get s pos in
              loop re ~abs_pos ~colors ~positions s ~pos:(pos + w) ~last st' st')
            else (
              (* Resize position array *)
              Positions.set positions (Idx.idx idx) (abs_pos + pos);
              let w = Cset.Codec.width String.unsafe_get s pos in
              loop re ~abs_pos ~colors ~positions s ~pos:(pos + w) ~last st' st')
          else if Idx.is_break idx then (
            Positions.set positions (Idx.break_idx idx) (abs_pos + pos);
            st')
          else (
            (* Unknown *)
            validate re s ~pos st0;
            loop re ~abs_pos ~colors ~positions s ~pos ~last st0 st0)
        else st

      let feed ({ t; positions; slices; abs_pos; first_match_pos = _ } as tt) s
        ~pos ~len =
        let state =
          (* TODO bound checks? *)
          let last = pos + len in
          loop t.re ~abs_pos ~colors:t.re.colors s ~positions ~last ~pos t.state
            t.state
        in
        let info = State.get_info state in
        if
          Idx.is_break info.idx
          &&
          match Automata.State.status t.re.mutex info.desc with
          | Failed -> true
          | Match _ | Running -> false
        then No_match
        else
          let t = { t with state } in
          let slices = { Slice.s; pos; len } :: slices in
          let first_match_pos = Positions.first positions in
          let slices =
            Slice.L.drop_rev slices (first_match_pos - tt.first_match_pos)
          in
          let abs_pos = abs_pos + len in
          Ok { tt with t; slices; abs_pos; first_match_pos }

      let finalize
        ({ t; positions; slices; abs_pos; first_match_pos = _ } as tt) s ~pos
        ~len : Match.t feed =
        (* TODO bound checks? *)
        let last = pos + len in
        let info =
          let state =
            loop t.re ~abs_pos ~colors:t.re.colors s ~positions ~last ~pos
              t.state t.state
          in
          State.get_info state
        in
        match
          match Automata.State.status t.re.mutex info.desc with
          | (Match _ | Failed) as s -> s
          | Running ->
            let idx, res =
              let final_cat = Category.(search_boundary ++ inexistant) in
              final t.re info final_cat
            in
            (match res with
            | Running | Failed -> ()
            | Match _ ->
              Positions.set positions (Automata.Idx.to_int idx) (abs_pos + last));
            res
        with
        | Running | Failed -> No_match
        | Match (marks, pmarks) ->
          let first_match_position = Positions.first positions in
          let slices =
            let slices =
              let slices = { Slice.s; pos; len } :: slices in
              Slice.L.drop_rev slices (first_match_position - tt.first_match_pos)
            in
            List.rev slices
          in
          Ok
            (Match.make ~start_pos:first_match_position ~pmarks ~marks ~slices
               ~positions)
    end
  end

  type match_info =
    | Match of Group.t
    | Failed
    | Running of { no_match_starts_before : int }

  let match_str_no_bounds ~groups ~partial re s ~pos ~len =
    let positions = Positions.make ~groups re in
    match make_match_str re positions ~len ~groups ~partial s ~pos with
    | Match (marks, pmarks) ->
      Match
        (Group.create s marks pmarks ~gpos:(Positions.all positions)
           ~gcount:re.group_count)
    | Failed -> Failed
    | Running ->
      let no_match_starts_before =
        if groups then Positions.first positions else 0
      in
      Running { no_match_starts_before }

  let match_str_p re s ~pos ~len =
    if pos < 0 || len < -1 || pos + len > String.length s then
      raise (Invalid_argument "Re.exec: out of bounds");
    match
      make_match_str re Positions.empty ~len ~groups:false ~partial:false s ~pos
    with
    | Match _ -> true
    | _ -> false

  let match_str ~groups ~partial re s ~pos ~len =
    if pos < 0 || len < -1 || pos + len > String.length s then
      invalid_arg "Re.exec: out of bounds";
    match_str_no_bounds ~groups ~partial re s ~pos ~len

  let mk_re ~initial ~colors ~color_repr ~ncolor ~lnl ~group_names ~group_count
      =
    {
      initial;
      initial_states = [];
      colors;
      color_repr;
      ncolor;
      lnl;
      tbl = Automata.Working_area.create ();
      states = Automata.State.Table.create 97;
      group_names;
      group_count;
      mutex = Mutex.create ();
    }

  (**** Compilation ****)

  (* module A = Automata *)

  let enforce_kind ids kind kind' cr =
    match (kind, kind') with
    | `First, `First -> cr
    | `First, k -> Automata.seq ids k cr (Automata.eps ids)
    | _ -> cr

  type context = {
    ids : Automata.Ids.t;
    kind : Automata.Sem.t;
    ign_group : bool;
    greedy : Automata.Rep_kind.t;
    pos : Automata.Mark.t ref;
    names : (string * int) list ref;
    cache : Cset.t Cset.CSetMap.t ref;
    colors : Color_map.Table.t;
  }

  let trans_set cache (cm : Color_map.Table.t) s =
    match Cset.one_c s with
    | Some i -> Cset.csingle (Color_map.Table.get_letter cm i)
    | None -> (
      let v = (Cset.hash s, s) in
      try Cset.CSetMap.find v !cache
      with Not_found ->
        let l = Color_map.Table.translate_colors cm s in
        cache := Cset.CSetMap.add v l !cache;
        l)

  let make_repeater ids cr kind greedy =
    match greedy with
    | `Greedy ->
      fun rem ->
        Automata.alt ids
          [
            Automata.seq ids kind (Automata.rename ids cr) rem; Automata.eps ids;
          ]
    | `Non_greedy ->
      fun rem ->
        Automata.alt ids
          [
            Automata.eps ids; Automata.seq ids kind (Automata.rename ids cr) rem;
          ]

  (* XXX should probably compute a category mask *)
  let rec translate
    ({ ids; kind; ign_group; greedy; pos; names; cache; colors } as ctx)
    (ast : Ast.no_case) =
    match ast with
    | Set s -> (Automata.cst ids (trans_set cache colors s), kind)
    | Sequence l -> (trans_seq ctx l, kind)
    | Ast (Alternative l) -> (
      match Ast.merge_sequences l with
      | [ r' ] ->
        let cr, kind' = translate ctx r' in
        (enforce_kind ids kind kind' cr, kind)
      | merged_sequences ->
        ( Automata.alt ids
            (List.map merged_sequences ~f:(fun r' ->
               let cr, kind' = translate ctx r' in
               enforce_kind ids kind kind' cr)),
          kind ))
    | Repeat (r', i, j) ->
      let cr, kind' = translate ctx r' in
      let rem =
        match j with
        | None -> Automata.rep ids greedy kind' cr
        | Some j ->
          let f = make_repeater ids cr kind' greedy in
          iter (j - i) f (Automata.eps ids)
      in
      ( iter i
          (fun rem -> Automata.seq ids kind' (Automata.rename ids cr) rem)
          rem,
        kind )
    | Beg_of_line -> (Automata.after ids Category.(inexistant ++ newline), kind)
    | End_of_line -> (Automata.before ids Category.(inexistant ++ newline), kind)
    | Beg_of_word ->
      ( Automata.seq ids `First
          (Automata.after ids Category.(inexistant ++ not_letter))
          (Automata.before ids Category.letter),
        kind )
    | End_of_word ->
      ( Automata.seq ids `First
          (Automata.after ids Category.letter)
          (Automata.before ids Category.(inexistant ++ not_letter)),
        kind )
    | Not_bound ->
      ( Automata.alt ids
          [
            Automata.seq ids `First
              (Automata.after ids Category.letter)
              (Automata.before ids Category.letter);
            (let cat = Category.(inexistant ++ not_letter) in
             Automata.seq ids `First (Automata.after ids cat)
               (Automata.before ids cat));
          ],
        kind )
    | Beg_of_str -> (Automata.after ids Category.inexistant, kind)
    | End_of_str -> (Automata.before ids Category.inexistant, kind)
    | Last_end_of_line ->
      (Automata.before ids Category.(inexistant ++ lastnewline), kind)
    | Start -> (Automata.after ids Category.search_boundary, kind)
    | Stop -> (Automata.before ids Category.search_boundary, kind)
    | Sem (kind', r') ->
      let cr, kind'' = translate { ctx with kind = kind' } r' in
      (enforce_kind ids kind' kind'' cr, kind')
    | Sem_greedy (greedy', r') -> translate { ctx with greedy = greedy' } r'
    | Group (n, r') ->
      if ign_group then translate ctx r'
      else
        let p = !pos in
        let () =
          match n with
          | Some name -> names := (name, Automata.Mark.group_count p) :: !names
          | None -> ()
        in
        pos := Automata.Mark.next2 !pos;
        let cr, kind' = translate ctx r' in
        ( Automata.seq ids `First (Automata.mark ids p)
            (Automata.seq ids `First cr
               (Automata.mark ids (Automata.Mark.next p))),
          kind' )
    | No_group r' -> translate { ctx with ign_group = true } r'
    | Nest r' ->
      let b = !pos in
      let cr, kind' = translate ctx r' in
      let e = Automata.Mark.prev !pos in
      if Automata.Mark.compare e b = -1 then (cr, kind')
      else (Automata.seq ids `First (Automata.erase ids b e) cr, kind')
    | Pmark (i, r') ->
      let cr, kind' = translate ctx r' in
      (Automata.seq ids `First (Automata.pmark ids i) cr, kind')

  and trans_seq ({ ids; kind; _ } as ctx) = function
    | [] -> Automata.eps ids
    | [ r ] ->
      let cr', kind' = translate ctx r in
      enforce_kind ids kind kind' cr'
    | r :: rem ->
      let cr', kind' = translate ctx r in
      let cr'' = trans_seq ctx rem in
      if Automata.is_eps cr'' then cr'
      else if Automata.is_eps cr' then cr''
      else Automata.seq ids kind' cr' cr''

  let compile_1 regexp =
    let regexp = Ast.handle_case false regexp in
    let color_map = Color_map.make () in
    let need_lnl = Ast.colorize color_map regexp in
    let colors, color_repr = Color_map.flatten color_map in
    let ncolor = Color_map.Repr.length color_repr in
    let lnl =
      if need_lnl then Cset.CodePage.of_int ncolor else Cset.CodePage.null
    in
    let ncolor = if need_lnl then ncolor + 1 else ncolor in
    let ctx =
      {
        ids = Automata.Ids.create ();
        kind = `First;
        ign_group = false;
        greedy = `Greedy;
        pos = ref Automata.Mark.start;
        names = ref [];
        cache = ref Cset.CSetMap.empty;
        colors;
      }
    in
    let r, kind = translate ctx regexp in
    let r = enforce_kind ctx.ids `First kind r in
    mk_re ~initial:r ~colors ~color_repr ~ncolor ~lnl
      ~group_names:(List.rev !(ctx.names))
      ~group_count:(Automata.Mark.group_count !(ctx.pos))

  let compile r =
    let open Ast.Export in
    compile_1
      (if Ast.anchored r then group r else seq [ shortest (rep any); group r ])
end
