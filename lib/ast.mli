type 'a ast = private
  | Alternative of 'a list
  | Sem of Automata.sem * 'a
  | Sem_greedy of Automata.rep_kind * 'a
  | No_group of 'a
  | No_case of 'a
  | Case of 'a

type cset = private
  | Cset of Cset.t
  | Intersection of cset list
  | Complement of cset list
  | Difference of cset * cset
  | Cast of cset ast

type t = private
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

val pp : t Fmt.t
val merge_sequences : t list -> t list
val handle_case : bool -> t -> t
val anchored : t -> bool
val colorize : Color_map.t -> t -> bool
val empty : t
val epsilon : t
val str : string -> t
val no_case : t -> t
val case : t -> t
val diff : t -> t -> t
val compl : t list -> t
val repn : t -> int -> int option -> t
val inter : t list -> t
val set : string -> t
val mark : t -> Pmark.t * t
val nest : t -> t
val no_group : t -> t
val whole_string : t -> t
val leol : t
val longest : t -> t
val greedy : t -> t
val non_greedy : t -> t
val stop : t
val not_boundary : t
val group : ?name:string -> t -> t
val word : t -> t
val first : t -> t
val bos : t
val bow : t
val eow : t
val eos : t
val bol : t
val start : t
val eol : t
val opt : t -> t
val rep : t -> t
val rep1 : t -> t
val alt : t list -> t
val shortest : t -> t
val seq : t list -> t
val cset : Cset.t -> t
val t_of_cset : cset -> t
