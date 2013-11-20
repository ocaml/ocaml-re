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

let rec first f l =
  match l with
    []     -> None
  | x :: r -> match f x with
                None          -> first f r
              | Some _ as res -> res

let rec iter n f v = if n = 0 then v else iter (n - 1) f (f v)

(****)

let unknown = -2
let break = -3

type 'a match_info =
  [ `Match of 'a
  | `Failed
  | `Running ]

type state =
  { idx : int;
        (* Index of the current position in the position table.
           Not yet computed transitions point to a dummy state where
           [idx] is set to [unknown];
           If [idx] is set to [break] for states that either always
           succeed or always fail. *)
    real_idx : int;
        (* The real index, in case [idx] is set to [break] *)
    next : state array;
        (* Transition table, indexed by color *)
    mutable final :
      (Automata.category *
       (Automata.idx * Automata.mark_infos match_info)) list;
        (* Mapping from the category of the next character to
           - the index where the next position should be saved
           - possibly, the list of marks (and the corresponding indices)
             corresponding to the best match *)
    desc : Automata.state
        (* Description of this state of the automata *) }

(* Automata (compiled regular expression) *)
type re =
  { initial : Automata.expr;
        (* The whole regular expression *)
    mutable initial_states : (int * state) list;
        (* Initial states, indexed by initial category *)
    cols : string;
        (* Color table *)
    col_repr : string;
        (* Table from colors to one character of this color *)
    ncol : int;
        (* Number of colors *)
    lnl : int;
        (* Color of the last newline *)
    mutable tbl : Automata.working_area;
        (* Temporary table used to compute the first available index
           when computing a new state *)
    states : state Automata.States.t;
        (* States of the deterministic automata *)
    group_count : int
        (* Number of groups in the regular expression *) }

let print_re ch re = Automata.print_expr ch re.initial

(* Information used during matching *)
type info =
  { re : re;
        (* The automata *)
    i_cols : string;
        (* Color table ([x.i_cols = x.re.cols])
           Sortcut used for performance reasons *)
    mutable positions : int array;
        (* Array of mark positions
           The mark are off by one for performance reasons *)
    mutable pos : int;
        (* Position where the match is started *)
    mutable last : int
        (* Position where the match should stop *) }

(****)

let cat_inexistant = 1
let cat_letter = 2
let cat_not_letter = 4
let cat_newline = 8
let cat_lastnewline = 16
let cat_search_boundary = 32

let category re c =
  if c = -1 then cat_inexistant else
  (* Special category for the last newline *)
  if c = re.lnl then cat_lastnewline lor cat_newline lor cat_not_letter else
  match re.col_repr.[c] with
    'a'..'z' | 'A'..'Z' ->
      cat_letter
  | '\n' ->
      cat_not_letter lor cat_newline
  | _ ->
      cat_not_letter

(****)

let dummy_next = [||]

let unknown_state =
  { idx = unknown; real_idx = 0;
    next = dummy_next; final = [];
    desc = Automata.dummy_state }

let count = ref 0
let mk_state ncol ((idx, _, _, _, _) as desc) =
  let break_state =
    match Automata.status desc with
      `Running -> false
    | _        -> true
  in
  { idx = if break_state then break else idx;
    real_idx = idx;
    next = if break_state then dummy_next else Array.make ncol unknown_state;
    final = [];
    desc = desc }

let find_state re desc =
  try
    Automata.States.find re.states desc
  with Not_found ->
    let st = mk_state re.ncol desc in
    Automata.States.add re.states desc st;
    st

(**** Match with marks ****)

let delta info cat c st =
  let (idx, _, _, _, _) as desc = Automata.delta info.re.tbl cat c st.desc in
  let len = Array.length info.positions in
  if idx = len && len > 0 then begin
    let pos = info.positions in
    info.positions <- Array.make (2 * len) 0;
    Array.blit pos 0 info.positions 0 len
  end;
  desc

let validate info s pos st =
  let c = Char.code info.i_cols.[Char.code s.[pos]] in
  let cat = category info.re c in
  let desc' = delta info cat c st in
  let st' = find_state info.re desc' in
  st.next.(c) <- st'

(*
let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then begin
      info.positions.(idx) <- pos;
      loop info s (pos + 1) st'
    end else if idx = break then begin
      info.positions.(st'.real_idx) <- pos;
      st'
    end else begin (* Unknown *)
      validate info s pos st;
      loop info s pos st
    end
  else
    st
*)

let rec loop info s pos st =
  if pos < info.last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    loop2 info s pos st st'
  else
    st

and loop2 info s pos st st' =
  let idx = st'.idx in
  if idx >= 0 then begin
    let pos = pos + 1 in
    if pos < info.last then begin
      (* It is important to place these reads before the write *)
      (* But then, we don't have enough registers left to store the
         right position.  So, we store the position plus one. *)
      let st'' = st'.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
      info.positions.(idx) <- pos;
      loop2 info s pos st' st''
    end else begin
      info.positions.(idx) <- pos;
      st'
    end
  end else if idx = break then begin
    info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    validate info s pos st;
    loop info s pos st
  end

let rec loop_no_mark info s pos last st =
  if pos < last then
    let st' = st.next.(Char.code info.i_cols.[Char.code s.[pos]]) in
    let idx = st'.idx in
    if idx >= 0 then
      loop_no_mark info s (pos + 1) last st'
    else if idx = break then
      st'
    else begin (* Unknown *)
      validate info s pos st;
      loop_no_mark info s pos last st
    end
  else
    st

let final info st cat =
  try
    List.assq cat st.final
  with Not_found ->
    let (idx, _, _, _, _) as st' = delta info cat (-1) st in
    let res = (idx, Automata.status st') in
    st.final <- (cat, res) :: st.final;
    res

let find_initial_state re cat =
  try
    List.assq cat re.initial_states
  with Not_found ->
    let st =
      find_state re (Automata.create_state cat re.initial)
    in
    re.initial_states <- (cat, st) :: re.initial_states;
    st

let dummy_substrings = `Match ("", [], [||], 0)

let get_color re s pos =
  if pos < 0 then -1 else
  let slen = String.length s in
  if pos >= slen then -1 else
  (* Special case for the last newline *)
  if pos = slen - 1 && re.lnl <> -1 && s.[pos] = '\n' then re.lnl else
  Char.code re.cols.[Char.code s.[pos]]

let rec handle_last_newline info pos st groups =
  let st' = st.next.(info.re.lnl) in
  let idx = st'.idx in
  if idx >= 0 then begin
    if groups then info.positions.(idx) <- pos + 1;
    st'
  end else if idx = break then begin
    if groups then info.positions.(st'.real_idx) <- pos + 1;
    st'
  end else begin (* Unknown *)
    let c = info.re.lnl in
    let real_c = Char.code info.i_cols.[Char.code '\n'] in
    let cat = category info.re c in
    let desc' = delta info cat real_c st in
    let st' = find_state info.re desc' in
    st.next.(c) <- st';
    handle_last_newline info pos st groups
  end

let rec scan_str info s initial_state groups =
  let pos = info.pos in
  let last = info.last in
  if
    last = String.length s &&
    info.re.lnl <> -1 &&
    last > pos &&
    s.[last - 1] = '\n'
  then begin
    info.last <- last - 1;
    let st = scan_str info s initial_state groups in
    if st.idx = break then
      st
    else
      handle_last_newline info (last - 1) st groups
  end else if groups then
    loop info s pos initial_state
  else
    loop_no_mark info s pos last initial_state

let match_str groups re s pos len =
  let slen = String.length s in
  let last = if len = -1 then slen else pos + len in
  let info =
    { re = re; i_cols = re.cols; pos = pos; last = last;
      positions =
        if groups then begin
          let n = Automata.index_count re.tbl + 1 in
          if n <= 10 then
            [|0;0;0;0;0;0;0;0;0;0|]
          else
          Array.make n 0
        end else
          [||] }
  in
  let initial_cat =
    if pos = 0 then
      cat_search_boundary lor cat_inexistant
    else
      cat_search_boundary lor category re (get_color re s (pos - 1)) in
  let initial_state = find_initial_state re initial_cat in
  let st = scan_str info s initial_state groups in
  let res =
    if st.idx = break then
      Automata.status st.desc
    else
      let final_cat =
        if last = slen then
          cat_search_boundary lor cat_inexistant
        else
          cat_search_boundary lor category re (get_color re s last) in
      let (idx, res) = final info st final_cat in
      if groups then info.positions.(idx) <- last + 1;
      res
  in
  match res with
    `Match m ->
      `Match (s, m, info.positions, re.group_count)
  | (`Failed | `Running) as res ->
      res

let mk_re init cols col_repr ncol lnl group_count =
  { initial = init;
    initial_states = [];
    cols = cols;
    col_repr = col_repr;
    ncol = ncol;
    lnl = lnl;
    tbl = Automata.create_working_area ();
    states = Automata.States.create 97;
    group_count = group_count }

(**** Character sets ****)

let cany = [0, 255]

let cseq c c' = Cset.seq (Char.code c) (Char.code c')
let cadd c s = Cset.add (Char.code c) s
let csingle c = Cset.single (Char.code c)

let rec interval i j = if i > j then [] else i :: interval (i + 1) j

let rec cset_hash_rec l =
  match l with
    []        -> 0
  | (i, j)::r -> i + 13 * j + 257 * cset_hash_rec r
let cset_hash l = (cset_hash_rec l) land 0x3FFFFFFF

module CSetMap =
  Map.Make
  (struct
    type t = int * (int * int) list
    let compare (i, u) (j, v) =
      let c = compare i j in if c <> 0 then c else compare u v
   end)

let trans_set cache cm s =
  match s with
    [i, j] when i = j ->
      csingle cm.[i]
  | _ ->
      let v = (cset_hash_rec s, s) in
      try
        CSetMap.find v !cache
      with Not_found ->
        let l =
          List.fold_right
            (fun (i, j) l -> Cset.union (cseq cm.[i] cm.[j]) l)
            s Cset.empty
        in
        cache := CSetMap.add v l !cache;
        l

(****)

type sem_status = Compulsory | Indicative

type regexp =
    Set of Cset.t
  | Sequence of regexp list
  | Alternative of regexp list
  | Repeat of regexp * int * int option
  | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str
  | Last_end_of_line | Start | Stop
  | Sem of Automata.sem * regexp
  | Sem_greedy of Automata.rep_kind * regexp
  | Group of regexp | No_group of regexp | Nest of regexp
  | Case of regexp | No_case of regexp
  | Intersection of regexp list
  | Complement of regexp list
  | Difference of regexp * regexp

let rec is_charset r =
  match r with
    Set _ ->
      true
  | Alternative l | Intersection l | Complement l ->
      List.for_all is_charset l
  | Difference (r, r') ->
      is_charset r && is_charset r'
  | Sem (_, r) | Sem_greedy (_, r)
  | No_group r | Case r | No_case r ->
      is_charset r
  | Sequence _ | Repeat _ | Beg_of_line | End_of_line
  | Beg_of_word | End_of_word | Beg_of_str | End_of_str
  | Not_bound | Last_end_of_line | Start | Stop | Group _ | Nest _ ->
      false

(**** Colormap ****)

(*XXX Use a better algorithm allowing non-contiguous regions? *)
let rec split s cm =
  match s with
    []    -> ()
  | (i, j)::r -> cm.[i] <- '\001'; cm.[j + 1] <- '\001'; split r cm

let cupper =
  Cset.union (cseq 'A' 'Z') (Cset.union (cseq '\192' '\214') (cseq '\216' '\222'))
let clower = Cset.offset 32 cupper
let calpha = cadd '\170' (cadd '\186' (Cset.union clower cupper))
let cdigit = cseq '0' '9'
let calnum = Cset.union calpha cdigit
let cword = cadd '_' calnum

let rec colorize c regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
      Set s                     -> split s c
    | Sequence l                -> List.iter colorize l
    | Alternative l             -> List.iter colorize l
    | Repeat (r, _, _)          -> colorize r
    | Beg_of_line | End_of_line -> split (csingle '\n') c
    | Beg_of_word | End_of_word
    | Not_bound                 -> split cword c
    | Beg_of_str | End_of_str
    | Start | Stop              -> ()
    | Last_end_of_line          -> lnl := true
    | Sem (_, r)
    | Sem_greedy (_, r)
    | Group r | No_group r
    | Nest r                    -> colorize r
    | Case _ | No_case _
    | Intersection _
    | Complement _
    | Difference _              -> assert false
  in
  colorize regexp;
  !lnl

let make_cmap () = String.make 257 '\000'

let flatten_cmap cm =
  let c = String.create 256 in
  let col_repr = String.create 256 in
  let v = ref 0 in
  c.[0] <- '\000';
  col_repr.[0] <- '\000';
  for i = 1 to 255 do
    if cm.[i] <> '\000' then incr v;
    c.[i] <- Char.chr !v;
    col_repr.[!v] <- Char.chr i
  done;
  (c, String.sub col_repr 0 (!v + 1), !v + 1)

(**** Compilation ****)

let sequence l =
  match l with
    [x] -> x
  | l   -> Sequence l

let rec merge_sequences l =
  match l with
    [] ->
      l
  | Alternative l' :: r ->
      merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
      begin match merge_sequences r with
        Sequence (x' :: y') :: r' when x = x' ->
          Sequence [x; Alternative [sequence y; sequence y']] :: r'
      | r' ->
          Sequence (x :: y) :: r'
      end
  | x :: r ->
      x :: merge_sequences r

module A = Automata

let enforce_kind ids kind kind' cr =
  match kind, kind' with
    `First, `First -> cr
  | `First, k       -> A.seq ids k cr (A.eps ids)
  |  _               -> cr

(* XXX should probably compute a category mask *)
let rec translate ids kind ign_group ign_case greedy pos cache c r =
  match r with
    Set s ->
      (A.cst ids (trans_set cache c s), kind)
  | Sequence l ->
      (trans_seq ids kind ign_group ign_case greedy pos cache c l, kind)
  | Alternative l ->
      begin match merge_sequences l with
        [r'] ->
          let (cr, kind') =
            translate ids kind ign_group ign_case greedy pos cache c r' in
          (enforce_kind ids kind kind' cr, kind)
      | l' ->
          (A.alt ids
             (List.map
                (fun r' ->
                   let (cr, kind') =
                     translate ids kind ign_group ign_case greedy
                       pos cache c r' in
                   enforce_kind ids kind kind' cr)
                (merge_sequences l)),
           kind)
      end
  | Repeat (r', i, j) ->
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r' in
      let rem =
        match j with
          None ->
            A.rep ids greedy kind' cr
        | Some j ->
            let f =
              match greedy with
                `Greedy ->
                  fun rem ->
                    A.alt ids
                      [A.seq ids kind' (A.rename ids cr) rem; A.eps ids]
              | `Non_greedy ->
                  fun rem ->
                    A.alt ids
                      [A.eps ids; A.seq ids kind' (A.rename ids cr) rem]
            in
            iter (j - i) f (A.eps ids)
      in
      (iter i (fun rem -> A.seq ids kind' (A.rename ids cr) rem) rem, kind)
  | Beg_of_line ->
      (A.after ids (cat_inexistant lor cat_newline), kind)
  | End_of_line ->
      (A.before ids (cat_inexistant lor cat_newline), kind)
  | Beg_of_word ->
      (A.seq ids `First
           (A.after ids (cat_inexistant lor cat_not_letter))
           (A.before ids (cat_inexistant lor cat_letter)),
       kind)
  | End_of_word ->
      (A.seq ids `First
           (A.after ids (cat_inexistant lor cat_letter))
           (A.before ids (cat_inexistant lor cat_not_letter)),
       kind)
  | Not_bound ->
      (A.alt ids [A.seq ids `First
                    (A.after ids cat_letter)
                    (A.before ids cat_letter);
                  A.seq ids `First
                    (A.after ids cat_letter)
                    (A.before ids cat_letter)],
       kind)
  | Beg_of_str ->
      (A.after ids cat_inexistant, kind)
  | End_of_str ->
      (A.before ids cat_inexistant, kind)
  | Last_end_of_line ->
      (A.before ids (cat_inexistant lor cat_lastnewline), kind)
  | Start ->
      (A.after ids cat_search_boundary, kind)
  | Stop ->
      (A.before ids cat_search_boundary, kind)
  | Sem (kind', r') ->
      let (cr, kind'') =
        translate ids kind' ign_group ign_case greedy pos cache c r' in
      (enforce_kind ids kind' kind'' cr,
       kind')
  | Sem_greedy (greedy', r') ->
      translate ids kind ign_group ign_case greedy' pos cache c r'
  | Group r' ->
      if ign_group then
        translate ids kind ign_group ign_case greedy pos cache c r'
      else
        let p = !pos in
        pos := !pos + 2;
        let (cr, kind') =
          translate ids kind ign_group ign_case greedy pos cache c r' in
        (A.seq ids `First (A.mark ids p) (
         A.seq ids `First cr (A.mark ids (p + 1))),
         kind')
  | No_group r' ->
      translate ids kind true ign_case greedy pos cache c r'
  | Nest r' ->
      let b = !pos in
      let (cr, kind') =
        translate ids kind ign_group ign_case greedy pos cache c r'
      in
      let e = !pos - 1 in
      if e < b then
        (cr, kind')
      else
        (A.seq ids `First (A.erase ids b e) cr, kind')
  | Difference _ | Complement _ | Intersection _ | No_case _ | Case _ ->
      assert false

and trans_seq ids kind ign_group ign_case greedy pos cache c l =
  match l with
    [] ->
      A.eps ids
  | [r] ->
      let (cr', kind') =
        translate ids kind ign_group ign_case greedy pos cache c r in
      enforce_kind ids kind kind' cr'
  | r :: rem ->
      let (cr', kind') =
        translate ids kind ign_group ign_case greedy pos cache c r in
      let cr'' =
        trans_seq ids kind ign_group ign_case greedy pos cache c rem in
      if A.def cr'' = A.Eps then
        cr'
      else if A.def cr' = A.Eps then
        cr''
      else
        A.seq ids kind' cr' cr''

(**** Case ****)

let case_insens s =
  Cset.union s (Cset.union (Cset.offset 32 (Cset.inter s cupper))
                   (Cset.offset (-32) (Cset.inter s clower)))

let as_set r =
  match r with
    Set s -> s
  | _     -> assert false

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case r =
  match r with
    Set s ->
      Set (if ign_case then case_insens s else s)
  | Sequence l ->
      Sequence (List.map (handle_case ign_case) l)
  | Alternative l ->
      let l' = List.map (handle_case ign_case) l in
      if is_charset (Alternative l') then
        Set (List.fold_left (fun s r -> Cset.union s (as_set r)) Cset.empty l')
      else
        Alternative l'
  | Repeat (r, i, j) ->
      Repeat (handle_case ign_case r, i, j)
  | Beg_of_line | End_of_line | Beg_of_word | End_of_word | Not_bound
  | Beg_of_str | End_of_str | Last_end_of_line | Start | Stop ->
      r
  | Sem (k, r) ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Sem (k, r')
  | Sem_greedy (k, r) ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Sem_greedy (k, r')
  | Group r ->
      Group (handle_case ign_case r)
  | No_group r ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      No_group r'
  | Nest r ->
      let r' = handle_case ign_case r in
      if is_charset r' then r' else
      Nest r'
  | Case r ->
      handle_case false r
  | No_case r ->
      handle_case true r
  | Intersection l ->
      let l' = List.map (fun r -> handle_case ign_case r) l in
      Set (List.fold_left (fun s r -> Cset.inter s (as_set r)) cany l')
  | Complement l ->
      let l' = List.map (fun r -> handle_case ign_case r) l in
      Set (Cset.diff cany
             (List.fold_left (fun s r -> Cset.union s (as_set r))
                Cset.empty l'))
  | Difference (r, r') ->
      Set (Cset.inter (as_set (handle_case ign_case r))
             (Cset.diff cany (as_set (handle_case ign_case r'))))

(****)

let compile_1 regexp =
  let regexp = handle_case false regexp in
  let c = make_cmap () in
  let need_lnl = colorize c regexp in
  let (col, col_repr, ncol) = flatten_cmap c in
  let lnl = if need_lnl then ncol else -1 in
  let ncol = if need_lnl then ncol + 1 else ncol in
  let ids = A.create_ids () in
  let pos = ref 0 in
  let (r, kind) =
    translate ids
      `First false false `Greedy pos (ref CSetMap.empty) col regexp in
  let r = enforce_kind ids `First kind r in
(*Format.eprintf "<%d %d>@." !ids ncol;*)
  mk_re r col col_repr ncol lnl (!pos / 2)

(****)

type t = regexp

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (csingle s.[i]) :: !l
  done;
  Sequence !l
let char c = Set (csingle c)

let alt l =
  match l with
    [r] -> r
  | _   -> Alternative l
let seq l =
  match l with
    [r] -> r
  | _   -> Sequence l
let empty = alt []
let epsilon = seq []
let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  begin match j with Some j when j < i -> invalid_arg "Re.repn" | _ -> () end;
  Repeat (r, i, j)
let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [bow; r; eow]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let whole_string r = seq [bos; r; eos]
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group r = Group r
let no_group r = No_group r
let nest r = Nest r

let set str =
  let s = ref [] in
  for i = 0 to String.length str - 1 do
    s := Cset.union (csingle str.[i]) !s
  done;
  Set !s

let rg c c' = Set (cseq c c')

let inter l =
  let r = Intersection l in
  if is_charset r then r else
  invalid_arg "Re.inter"

let compl l =
  let r = Complement l in
  if is_charset r then r else
  invalid_arg "Re.compl"

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then r'' else
  invalid_arg "Re.diff"

let any = Set cany
let notnl = Set (Cset.diff cany (csingle '\n'))

let lower = alt [rg 'a' 'z'; char '\181'; rg '\223' '\246'; rg '\248' '\255']
let upper = alt [rg 'A' 'Z'; rg '\192' '\214'; rg '\216' '\222']
let alpha = alt [lower; upper; char '\170'; char '\186']
let digit = rg '0' '9'
let alnum = alt [alpha; digit]
let ascii = rg '\000' '\127'
let blank = set "\t "
let cntrl = alt [rg '\000' '\031'; rg '\127' '\159']
let graph = alt [rg '\033' '\126'; rg '\160' '\255']
let print = alt [rg '\032' '\126'; rg '\160' '\255']
let punct =
  alt [rg '\033' '\047'; rg '\058' '\064'; rg '\091' '\096';
       rg '\123' '\126'; rg '\160' '\169'; rg '\171' '\180';
       rg '\182' '\185'; rg '\187' '\191'; char '\215'; char '\247']
let space = alt [char ' '; rg '\009' '\013']
let xdigit = alt [digit; rg 'a' 'f'; rg 'A' 'Z']

let case r = Case r
let no_case r = No_case r

(****)

type substrings = (string * Automata.mark_infos * int array * int)

let compile r = compile_1 (seq [shortest (rep any); group r])

let exec ?(pos = 0) ?(len = -1) re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.exec";
  match match_str true re s pos len with
    `Match substr -> substr
  | _             -> raise Not_found

let execp ?(pos = 0) ?(len = -1) re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.execp";
  match match_str false re s pos len with
    `Match substr -> true
  | _             -> false

let exec_partial ?(pos = 0) ?(len = -1) re s =
  if pos < 0 || len < -1 || pos + len > String.length s then
    invalid_arg "Re.exec_partial";
  match match_str false re s pos len with
    `Match _ -> `Full
  | `Running -> `Partial
  | `Failed  -> `Mismatch

let rec find_mark (i : int) l =
  match l with
    [] ->
      raise Not_found
  | (j, idx) :: r ->
      if i = j then idx else find_mark i r

let get (s, marks, pos, _) i =
  if 2 * i + 1 >= Array.length marks then raise Not_found;
  let m1 = marks.(2 * i) in
  if m1 = -1 then raise Not_found;
  let p1 = pos.(m1) - 1 in
  let p2 = pos.(marks.(2 * i + 1)) - 1 in
  String.sub s p1 (p2 - p1)

let get_ofs (s, marks, pos, _) i =
  if 2 * i + 1 >= Array.length marks then raise Not_found;
  let m1 = marks.(2 * i) in
  if m1 = -1 then raise Not_found;
  let p1 = pos.(m1) - 1 in
  let p2 = pos.(marks.(2 * i + 1)) - 1 in
  (p1, p2)

let test (s, marks, pos, _) i =
  if 2 * i >= Array.length marks then false else
  let idx = marks.(2 * i) in
  idx <> -1

let dummy_offset = (-1, -1)

let get_all_ofs (s, marks, pos, count) =
  let res = Array.make count dummy_offset in
  for i = 0 to Array.length marks / 2 - 1 do
    let m1 = marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = pos.(m1) in
      let p2 = pos.(marks.(2 * i + 1)) in
      res.(i) <- (p1 - 1, p2 - 1)
    end
  done;
  res

let dummy_string = ""

let get_all (s, marks, pos, count) =
  let res = Array.make count dummy_string in
  for i = 0 to Array.length marks / 2 - 1 do
    let m1 = marks.(2 * i) in
    if m1 <> -1 then begin
      let p1 = pos.(m1) in
      let p2 = pos.(marks.(2 * i + 1)) in
      res.(i) <- String.sub s (p1 - 1) (p2 - p1)
    end
  done;
  res

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
