open Import

type 'a ast =
  | Alternative of 'a list
  | Sem of Automata.sem * 'a
  | Sem_greedy of Automata.rep_kind * 'a
  | No_group of 'a
  | No_case of 'a
  | Case of 'a

let equal_ast eq x y =
  match x, y with
  | Alternative a, Alternative b -> List.equal ~eq a b
  | Sem (sem, a), Sem (sem', a') -> sem = sem' && eq a a'
  | Sem_greedy (rep, a), Sem_greedy (rep', a') -> rep = rep' && eq a a'
  | No_group a, No_group b -> eq a b
  | No_case a, No_case b -> eq a b
  | Case a, Case b -> eq a b
  | _, _ -> false
;;

let pp_ast f fmt ast =
  let open Fmt in
  let var s re = sexp fmt s f re in
  match ast with
  | Alternative alt -> sexp fmt "Alternative" (list f) alt
  | Sem (sem, a) -> sexp fmt "Sem" (pair Automata.pp_sem f) (sem, a)
  | Sem_greedy (k, re) -> sexp fmt "Sem_greedy" (pair Automata.pp_rep_kind f) (k, re)
  | No_group c -> var "No_group" c
  | Case c -> var "Case" c
  | No_case c -> var "No_case" c
;;

type cset =
  | Cset of Cset.t
  | Intersection of cset list
  | Complement of cset list
  | Difference of cset * cset
  | Cast of cset ast

type t =
  | Set of cset
  | Ast of t ast
  | Sequence of t list
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
  | Group of string option * t
  | Nest of t
  | Pmark of Pmark.t * t

let rec pp fmt t =
  let open Format in
  let open Fmt in
  let var s re = sexp fmt s pp re in
  let seq s rel = sexp fmt s (list pp) rel in
  match t with
  | Set cset -> pp_cset fmt cset
  | Sequence sq -> seq "Sequence" sq
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
  | Group (None, c) -> var "Group" c
  | Group (Some n, c) -> sexp fmt "Named_group" (pair str pp) (n, c)
  | Nest c -> var "Nest" c
  | Pmark (m, r) -> sexp fmt "Pmark" (pair Pmark.pp pp) (m, r)
  | Ast a -> pp_ast pp fmt a

and pp_cset fmt cset =
  let open Fmt in
  let seq s rel = sexp fmt s (list pp_cset) rel in
  match cset with
  | Cast s -> pp_ast pp_cset fmt s
  | Cset s -> sexp fmt "Set" Cset.pp s
  | Intersection c -> seq "Intersection" c
  | Complement c -> seq "Complement" c
  | Difference (a, b) -> sexp fmt "Difference" (pair pp_cset pp_cset) (a, b)
;;

let rec equal x1 x2 =
  match x1, x2 with
  | Set s1, Set s2 -> equal_cset s1 s2
  | Sequence l1, Sequence l2 -> List.equal ~eq:equal l1 l2
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
  | Group _, Group _ ->
    (* Do not merge groups! *)
    false
  | Pmark (m1, r1), Pmark (m2, r2) -> Pmark.equal m1 m2 && equal r1 r2
  | Nest x, Nest y -> equal x y
  | Ast x, Ast y -> equal_ast equal x y
  | _ -> false

and equal_cset (x : cset) (y : cset) : bool =
  match x, y with
  | Cset x, Cset y -> x = y
  | Intersection x, Intersection y -> List.equal ~eq:equal_cset x y
  | Complement x, Complement y -> List.equal ~eq:equal_cset x y
  | Difference (x, y), Difference (x', y') -> equal_cset x x' && equal_cset y y'
  | Cast x, Cast y -> equal_ast equal_cset x y
  | _, _ -> false
;;

let seq = function
  | [ r ] -> r
  | l -> Sequence l
;;

let rec merge_sequences = function
  | [] -> []
  | Ast (Alternative l') :: r -> merge_sequences (l' @ r)
  | Sequence (x :: y) :: r ->
    (match merge_sequences r with
     | Sequence (x' :: y') :: r' when equal x x' ->
       Sequence [ x; Ast (Alternative [ seq y; seq y' ]) ] :: r'
     | r' -> Sequence (x :: y) :: r')
  | x :: r -> x :: merge_sequences r
;;

let str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := Set (Cset (Cset.csingle s.[i])) :: !l
  done;
  seq !l
;;

let as_set_elems elems =
  match
    List.map elems ~f:(function
      | Set e -> e
      | _ -> raise_notrace Exit)
  with
  | exception Exit -> None
  | e -> Some e
;;

let empty = Ast (Alternative [])

let alt (elems : t list) : t =
  match elems with
  | [] -> empty
  | [ x ] -> x
  | _ ->
    (match as_set_elems elems with
     | None -> Ast (Alternative elems)
     | Some elems -> Set (Cast (Alternative elems)))
;;

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

type f = { f : 'a. 'a -> 'a ast }

let make_set f t =
  match t with
  | Set x -> Set (Cast (f.f x))
  | _ -> Ast (f.f t)
;;

let longest =
  let f = { f = (fun x -> Sem (`Longest, x)) } in
  fun t -> make_set f t
;;

let shortest =
  let f = { f = (fun r -> Sem (`Shortest, r)) } in
  fun t -> make_set f t
;;

let first =
  let f = { f = (fun r -> Sem (`First, r)) } in
  fun t -> make_set f t
;;

let greedy =
  let f = { f = (fun r -> Sem_greedy (`Greedy, r)) } in
  fun t -> make_set f t
;;

let non_greedy =
  let f = { f = (fun r -> Sem_greedy (`Non_greedy, r)) } in
  fun t -> make_set f t
;;

let group ?name r = Group (name, r)

let no_group =
  let f = { f = (fun r -> No_group r) } in
  fun t -> make_set f t
;;

let nest r = Nest r
let cset cset = Set (Cset cset)
let set str = cset (Cset.set str)

let mark r =
  let i = Pmark.gen () in
  i, Pmark (i, r)
;;

(**** Character sets ****)
let as_set_or_error name elems =
  match as_set_elems elems with
  | None -> invalid_arg name
  | Some s -> s
;;

let inter elems = Set (Intersection (as_set_or_error "Re.inter" elems))
let compl elems = Set (Complement (as_set_or_error "Re.compl" elems))

let diff r r' =
  match r, r' with
  | Set r, Set r' -> Set (Difference (r, r'))
  | _, _ -> invalid_arg "Re.diff"
;;

(****)

let case =
  let f = { f = (fun r -> Case r) } in
  fun t -> make_set f t
;;

let no_case =
  let f = { f = (fun r -> No_case r) } in
  fun t -> make_set f t
;;

(*XXX Use a better algorithm allowing non-contiguous regions? *)

let colorize color_map regexp =
  let lnl = ref false in
  let rec colorize regexp =
    match regexp with
    | Set (Cset s) -> Color_map.split color_map s
    | Sequence l -> List.iter ~f:colorize l
    | Ast (Alternative l) -> List.iter ~f:colorize l
    | Repeat (r, _, _) -> colorize r
    | Beg_of_line | End_of_line -> Color_map.split color_map Cset.nl
    | Beg_of_word | End_of_word | Not_bound -> Color_map.split color_map Cset.cword
    | Beg_of_str | End_of_str | Start | Stop -> ()
    | Last_end_of_line -> lnl := true
    | Group (_, r) | Nest r | Pmark (_, r) -> colorize r
    | Ast (Sem (_, r) | Sem_greedy (_, r) | No_group r) -> colorize r
    (* case handling eliminated before colorization *)
    | Ast (No_case _) | Ast (Case _) -> assert false
    (* all sets have been simplified to their primitive representations *)
    | Set (Intersection _ | Complement _ | Difference _) -> assert false
    | Set (Cast _) -> assert false
  in
  colorize regexp;
  !lnl
;;

let rec anchored_ast : t ast -> bool = function
  | Alternative als -> List.for_all ~f:anchored als
  | Sem (_, r) | Sem_greedy (_, r) | No_group r | No_case r | Case r -> anchored r

and anchored : t -> bool = function
  | Ast a -> anchored_ast a
  | Sequence l -> List.exists ~f:anchored l
  | Repeat (r, i, _) -> i > 0 && anchored r
  | Group (_, r) | Nest r | Pmark (_, r) -> anchored r
  | Set _
  | Beg_of_line
  | End_of_line
  | Beg_of_word
  | End_of_word
  | Not_bound
  | End_of_str
  | Last_end_of_line
  | Stop -> false
  | Beg_of_str | Start -> true
;;

let rec handle_case_cset ign_case = function
  | Cset s -> if ign_case then Cset.case_insens s else s
  | Cast (Alternative l) -> List.map ~f:(handle_case_cset ign_case) l |> Cset.union_all
  | Complement l ->
    List.map ~f:(handle_case_cset ign_case) l |> Cset.union_all |> Cset.diff Cset.cany
  | Difference (r, r') ->
    Cset.inter
      (handle_case_cset ign_case r)
      (Cset.diff Cset.cany (handle_case_cset ign_case r'))
  | Cast (No_group a) | Cast (Sem (_, a)) | Cast (Sem_greedy (_, a)) ->
    handle_case_cset ign_case a
  | Intersection l -> List.map ~f:(handle_case_cset ign_case) l |> Cset.intersect_all
  | Cast (No_case a) -> handle_case_cset true a
  | Cast (Case a) -> handle_case_cset false a
;;

(* CR rgrinberg: this function eliminates [Case]/[No_case] and simplifies
   all char sets to their primitive representation. We should reflect that in
   the types *)
let rec handle_case ign_case = function
  | Set s -> Set (Cset (handle_case_cset ign_case s))
  | Sequence l -> Sequence (List.map ~f:(handle_case ign_case) l)
  | Ast (Alternative l) ->
    let l = List.map ~f:(handle_case ign_case) l in
    Ast (Alternative l)
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
  | Ast (Sem (k, r)) ->
    let r = handle_case ign_case r in
    Ast (Sem (k, r))
  | Ast (Sem_greedy (k, r)) -> Ast (Sem_greedy (k, handle_case ign_case r))
  | Group (n, r) -> Group (n, handle_case ign_case r)
  | Ast (No_group r) -> Ast (No_group (handle_case ign_case r))
  | Nest r -> Nest (handle_case ign_case r)
  | Ast (Case r) -> handle_case false r
  | Ast (No_case r) -> handle_case true r
  | Pmark (i, r) -> Pmark (i, handle_case ign_case r)
;;

let t_of_cset x = Set x
