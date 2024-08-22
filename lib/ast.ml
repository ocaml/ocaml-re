open Import

type t =
  | Set of Cset.t
  | Sequence of t list
  | Alternative of t list
  | Repeat of t * int * int option
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | Beg_of_str
  | End_of_str
  | Last_end_of_line
  | Start
  | Stop
  | Sem of Automata.sem * t
  | Sem_greedy of Automata.rep_kind * t
  | Group of string option * t
  | No_group of t
  | Nest of t
  | Case of t
  | No_case of t
  | Intersection of t list
  | Complement of t list
  | Difference of t * t
  | Pmark of Pmark.t * t

let rec pp fmt t =
  let open Format in
  let open Fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set s -> sexp fmt "Set" Cset.pp s
  | Sequence sq -> seq "Sequence" sq
  | Alternative alt -> seq "Alternative" alt
  | Repeat (re, start, stop) ->
    let pp' fmt () = fprintf fmt "%a@ %d%a" pp re start optint stop in
    sexp fmt "Repeat" pp' ()
  | Beg_of_line -> str fmt "Beg_of_line"
  | End_of_line -> str fmt "End_of_line"
  | Beg_of_word -> str fmt "Beg_of_word"
  | End_of_word -> str fmt "End_of_word"
  | Not_bound -> str fmt "Not_bound"
  | Beg_of_str -> str fmt "Beg_of_str"
  | End_of_str -> str fmt "End_of_str"
  | Last_end_of_line -> str fmt "Last_end_of_line"
  | Start -> str fmt "Start"
  | Stop -> str fmt "Stop"
  | Sem (sem, re) -> sexp fmt "Sem" (pair Automata.pp_sem pp) (sem, re)
  | Sem_greedy (k, re) -> sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind pp) (k, re)
  | Group (None, c) -> var "Group" c
  | Group (Some n, c) -> sexp fmt "Named_group" (pair str pp) (n, c)
  | No_group c -> var "No_group" c
  | Nest c -> var "Nest" c
  | Case c -> var "Case" c
  | No_case c -> var "No_case" c
  | Intersection c -> seq "Intersection" c
  | Complement c -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp pp) (a, b)
  | Pmark (m, r) -> sexp fmt "Pmark" (pair Pmark.pp pp) (m, r)
;;

let rec is_charset = function
  | Set _ -> true
  | Alternative l -> List.for_all l ~f:is_charset
  | Intersection _ | Complement _ | Difference _ -> true
  | Sem (_, r) | Sem_greedy (_, r) | No_group r | Case r | No_case r -> is_charset r
  | Sequence _
  | Repeat _
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Beg_of_str
  | End_of_str
  | Not_bound
  | Last_end_of_line
  | Start
  | Stop
  | Group _
  | Nest _
  | Pmark (_, _) -> false
;;

let as_set = function
  | Set s -> s
  | _ -> assert false
;;

let rec equal x1 x2 =
  match x1, x2 with
  | Set s1, Set s2 -> s1 = s2
  | Sequence l1, Sequence l2 -> List.equal ~eq:equal l1 l2
  | Alternative l1, Alternative l2 -> List.equal ~eq:equal l1 l2
  | Repeat (x1', i1, j1), Repeat (x2', i2, j2) -> i1 = i2 && j1 = j2 && equal x1' x2'
  | Beg_of_line, Beg_of_line
  | End_of_line, End_of_line
  | Beg_of_word, Beg_of_word
  | End_of_word, End_of_word
  | Not_bound, Not_bound
  | Beg_of_str, Beg_of_str
  | End_of_str, End_of_str
  | Last_end_of_line, Last_end_of_line
  | Start, Start
  | Stop, Stop -> true
  | Sem (sem1, x1'), Sem (sem2, x2') -> sem1 = sem2 && equal x1' x2'
  | Sem_greedy (k1, x1'), Sem_greedy (k2, x2') -> k1 = k2 && equal x1' x2'
  | Group _, Group _ ->
    (* Do not merge groups! *)
    false
  | No_group x1', No_group x2' -> equal x1' x2'
  | Nest x1', Nest x2' -> equal x1' x2'
  | Case x1', Case x2' -> equal x1' x2'
  | No_case x1', No_case x2' -> equal x1' x2'
  | Intersection l1, Intersection l2 -> List.equal ~eq:equal l1 l2
  | Complement l1, Complement l2 -> List.equal ~eq:equal l1 l2
  | Difference (x1', x1''), Difference (x2', x2'') -> equal x1' x2' && equal x1'' x2''
  | Pmark (m1, r1), Pmark (m2, r2) -> Pmark.equal m1 m2 && equal r1 r2
  | _ -> false
;;

let seq = function
  | [ r ] -> r
  | l -> Sequence l
;;

let rec merge_sequences = function
  | [] -> []
  | Alternative l' :: r -> merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
    (match merge_sequences r with
     | Sequence (x' :: y') :: r' when equal x x' ->
       Sequence [ x; Alternative [ seq y; seq y' ] ] :: r'
     | r' -> Sequence (x :: y) :: r')
  | x :: r -> x :: merge_sequences r
;;

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (Cset.csingle s.[i]) :: !l
  done;
  seq !l
;;

let alt = function
  | [ r ] -> r
  | l -> Alternative l
;;

let empty = alt []
let epsilon = seq []

let repn r i j =
  if i < 0 then invalid_arg "Re.repn";
  match j, i with
  | Some j, _ when j < i -> invalid_arg "Re.repn"
  | Some 0, 0 -> epsilon
  | Some 1, 1 -> r
  | _ -> Repeat (r, i, j)
;;

let rep r = repn r 0 None
let rep1 r = repn r 1 None
let opt r = repn r 0 (Some 1)
let bol = Beg_of_line
let eol = End_of_line
let bow = Beg_of_word
let eow = End_of_word
let word r = seq [ bow; r; eow ]
let not_boundary = Not_bound
let bos = Beg_of_str
let eos = End_of_str
let whole_string r = seq [ bos; r; eos ]
let leol = Last_end_of_line
let start = Start
let stop = Stop
let longest r = Sem (`Longest, r)
let shortest r = Sem (`Shortest, r)
let first r = Sem (`First, r)
let greedy r = Sem_greedy (`Greedy, r)
let non_greedy r = Sem_greedy (`Non_greedy, r)
let group ?name r = Group (name, r)
let no_group r = No_group r
let nest r = Nest r
let set str = Set (Cset.set str)

let mark r =
  let i = Pmark.gen () in
  i, Pmark (i, r)
;;

(**** Character sets ****)

let inter l =
  let r = Intersection l in
  if is_charset r then r else invalid_arg "Re.inter"
;;

let compl l =
  let r = Complement l in
  if is_charset r then r else invalid_arg "Re.compl"
;;

let diff r r' =
  let r'' = Difference (r, r') in
  if is_charset r'' then r'' else invalid_arg "Re.diff"
;;

(****)

let case r = Case r
let no_case r = No_case r

(*XXX Use a better algorithm allowing non-contiguous regions? *)

let colorize color_map regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
    | Set s -> Color_map.split color_map s
    | Sequence l -> List.iter ~f:colorize l
    | Alternative l -> List.iter ~f:colorize l
    | Repeat (r, _, _) -> colorize r
    | Beg_of_line | End_of_line -> Color_map.split color_map Cset.nl
    | Beg_of_word | End_of_word | Not_bound -> Color_map.split color_map Cset.cword
    | Beg_of_str | End_of_str | Start | Stop -> ()
    | Last_end_of_line -> lnl := true
    | Sem (_, r) | Sem_greedy (_, r) | Group (_, r) | No_group r | Nest r | Pmark (_, r)
      -> colorize r
    | Case _ | No_case _ | Intersection _ | Complement _ | Difference _ -> assert false
  in
  colorize regexp;
  !lnl
;;

let rec anchored = function
  | Sequence l -> List.exists ~f:anchored l
  | Alternative l -> List.for_all ~f:anchored l
  | Repeat (r, i, _) -> i > 0 && anchored r
  | Set _
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | End_of_str
  | Last_end_of_line
  | Stop
  | Intersection _
  | Complement _
  | Difference _ -> false
  | Beg_of_str | Start -> true
  | Sem (_, r)
  | Sem_greedy (_, r)
  | Group (_, r)
  | No_group r
  | Nest r
  | Case r
  | No_case r
  | Pmark (_, r) -> anchored r
;;

(* XXX Should split alternatives into (1) charsets and (2) more
   complex regular expressions; alternative should therefore probably
   be flatten here *)
let rec handle_case ign_case = function
  | Set s -> Set (if ign_case then Cset.case_insens s else s)
  | Sequence l -> Sequence (List.map ~f:(handle_case ign_case) l)
  | Alternative l ->
    let l = List.map ~f:(handle_case ign_case) l in
    if is_charset (Alternative l)
    then Set (List.map ~f:as_set l |> Cset.union_all)
    else Alternative l
  | Repeat (r, i, j) -> Repeat (handle_case ign_case r, i, j)
  | ( Beg_of_line
    | End_of_line
    | Beg_of_word
    | End_of_word
    | Not_bound
    | Beg_of_str
    | End_of_str
    | Last_end_of_line
    | Start
    | Stop ) as r -> r
  | Sem (k, r) ->
    let r = handle_case ign_case r in
    if is_charset r then r else Sem (k, r)
  | Sem_greedy (k, r) ->
    let r = handle_case ign_case r in
    if is_charset r then r else Sem_greedy (k, r)
  | Group (n, r) -> Group (n, handle_case ign_case r)
  | No_group r ->
    let r = handle_case ign_case r in
    if is_charset r then r else No_group r
  | Nest r ->
    let r = handle_case ign_case r in
    if is_charset r then r else Nest r
  | Case r -> handle_case false r
  | No_case r -> handle_case true r
  | Intersection l ->
    Set
      (List.map ~f:(fun set -> handle_case ign_case set |> as_set) l |> Cset.intersect_all)
  | Complement l ->
    Set
      (List.map ~f:(fun s -> handle_case ign_case s |> as_set) l
       |> Cset.union_all
       |> Cset.diff Cset.cany)
  | Difference (r, r') ->
    Set
      (Cset.inter
         (as_set (handle_case ign_case r))
         (Cset.diff Cset.cany (as_set (handle_case ign_case r'))))
  | Pmark (i, r) -> Pmark (i, handle_case ign_case r)
;;
