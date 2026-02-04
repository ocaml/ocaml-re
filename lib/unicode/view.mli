module type T = sig
  (** A view of the top-level of a regex. This type is unstable and may change
  *)
  type letter

  type ast

  module Cset : sig
    type t

    module Range : sig
      type t

      val first : t -> letter
      val last : t -> letter
    end

    val view : t -> Range.t list
  end

  module Sem : sig
    type t = [ `Longest | `Shortest | `First ]
  end

  module Rep_kind : sig
    type t = [ `Greedy | `Non_greedy ]
  end

  type view =
    | Set of Cset.t
    | Sequence of ast list
    | Alternative of ast list
    | Repeat of ast * int * int option
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
    | Sem of Sem.t * ast
    | Sem_greedy of Rep_kind.t * ast
    | Group of string option * ast
    | No_group of ast
    | Nest of ast
    | Case of ast
    | No_case of ast
    | Intersection of ast list
    | Complement of ast list
    | Difference of ast * ast
    | Pmark of Pmark.t * ast

  val view : ast -> view
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) :
  T
    with type ast = Ast.Make(Cset)(Color_map).t
     and type Cset.t = Cset.t
     and type letter = Cset.letter
