module type T = sig
  module Cset : Cset.T

  module Color_map :
    Color_map.T
      with type cp := Cset.cp
       and type letter := Cset.letter
       and type cset_t := Cset.t

  module Re : sig
    include Core.T with type letter := Cset.letter
    include Replace.T with type re := re
    module View : View.T with type letter := Cset.letter and type ast := t
    module Emacs : Emacs.T with type core := t and type re := re
    module Glob : Glob.T with type core := t
    module Perl : Perl.T with type core := t and type re := re

    module Pcre :
      Pcre.T with type core := t and type re := re and type groups := Group.t

    module Posix : Posix.T with type core := t and type re := re
    module Str : Str.T
  end
end

module Make
    (Cset : Cset.T)
    (_ :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) : T

module Utf8 : T with type Cset.letter := Uchar.t
module Utf16be : T with type Cset.letter := Uchar.t
module Utf16le : T with type Cset.letter := Uchar.t
module Latin1 : T with type Cset.letter := Char.t

(**/**)
module Private : sig
  include module type of Import

  module Fmt : sig
    include module type of Fmt
  end

  module Dyn : sig
    include module type of Dyn
  end

  module Cset : sig
    include Cset.T with type letter = Uchar.t
  end

  module Color_map : sig
    include
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t
  end

  module Re : sig
    module Category : sig
      include Category.T with type letter = Cset.letter
    end

    module Automata : sig
      module Ids : sig
        type t

        val create : unit -> t
      end

      module Sem : sig
        type t = [ `Longest | `Shortest | `First ]

        val to_dyn : t -> Dyn.t
        val pp : t Fmt.t
      end

      module Rep_kind : sig
        type t = [ `Greedy | `Non_greedy ]

        val to_dyn : t -> Dyn.t
        val pp : t Fmt.t
      end

      module Mark : sig
        type t = private int

        val compare : t -> t -> int
        val equal : t -> t -> bool
        val pp : t Fmt.t
        val to_dyn : t -> Dyn.t
        val start : t
        val prev : t -> t
        val next : t -> t
        val next2 : t -> t
        val group_count : t -> int
        val outside_range : t -> start_inclusive:t -> stop_inclusive:t -> bool
      end

      module Idx : sig
        type t

        val to_int : t -> int
      end

      module Status : sig
        type t = Failed | Match of Mark_infos.t * Pmark.Set.t | Running
      end

      module type T = sig
        type cset
        type cp
        type category
        type expr

        val is_eps : expr -> bool
        val pp : expr Fmt.t
        val cst : Ids.t -> cset -> expr
        val empty : Ids.t -> expr
        val alt : Ids.t -> expr list -> expr
        val seq : Ids.t -> Sem.t -> expr -> expr -> expr
        val eps : Ids.t -> expr
        val rep : Ids.t -> Rep_kind.t -> Sem.t -> expr -> expr
        val mark : Ids.t -> Mark.t -> expr
        val pmark : Ids.t -> Pmark.t -> expr
        val erase : Ids.t -> Mark.t -> Mark.t -> expr
        val before : Ids.t -> category -> expr
        val after : Ids.t -> category -> expr
        val rename : Ids.t -> expr -> expr

        (****)

        (* States of the automata *)

        module State : sig
          type t

          val pp : t Fmt.t
          val dummy : t
          val create : category -> expr -> t
          val idx : t -> Idx.t
          val status_no_mutex : t -> Status.t
          val status : Mutex.t -> t -> Status.t
          val to_dyn : t -> Dyn.t

          module Table : Hashtbl.S with type key = t
        end

        (****)

        (* Computation of the states following a given state *)

        module Working_area : sig
          type t

          val create : unit -> t
          val index_count : t -> int
        end

        val delta : Working_area.t -> category -> cp -> State.t -> State.t
      end

      include
        T
          with type cset = Cset.t
           and type cp = Cset.cp
           and type category = Category.t
    end

    module Ast : sig
      type ('a, _) ast = private
        | Alternative : 'a list -> ('a, [> `Uncased ]) ast
        | No_case : 'a -> ('a, [> `Cased ]) ast
        | Case : 'a -> ('a, [> `Cased ]) ast

      type ('a, 'case) gen = private
        | Set of 'a
        | Ast of (('a, 'case) gen, 'case) ast
        | Sequence of ('a, 'case) gen list
        | Repeat of ('a, 'case) gen * int * int option
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
        | Group of string option * ('a, 'case) gen
        | No_group of ('a, 'case) gen
        | Nest of ('a, 'case) gen
        | Pmark of Pmark.t * ('a, 'case) gen
        | Sem of Automata.Sem.t * ('a, 'case) gen
        | Sem_greedy of Automata.Rep_kind.t * ('a, 'case) gen

      include
        Ast.T
          with type letter = Cset.letter
           and type cset_t = Cset.t
           and type color_map_t = Color_map.t
    end

    module Compile : sig
      include Compile.T with type ast = Ast.t
    end

    module Core : sig
      include
        Core.T
          with type t = Ast.t
           and type re = Compile.re
           and type letter = Cset.letter
    end

    module Replace : sig
      include Replace.T with type re = Compile.re
    end

    include
      module type of Core
        with type t = Ast.t
         and type re = Compile.re
         and type letter = Cset.letter

    include module type of Replace with type re := Compile.re

    module View :
      View.T with type ast := Ast.t and type letter := Cset.letter

    module Emacs : Emacs.T with type core := Core.t and type re := Compile.re
    module Glob : Glob.T with type core := Core.t
    module Perl : Perl.T with type core := Core.t and type re := Compile.re

    module Pcre :
      Pcre.T
        with type core := Core.t
         and type re := Compile.re
         and type groups := Group.t

    module Posix : Posix.T with type core := Core.t and type re := Compile.re
    module Str : Str.T
  end
end
(* module Utf16be : sig
  module Re : T with type letter := Uchar.t
end

module Utf16le : sig
  module Re : T with type letter := Uchar.t
end *)
