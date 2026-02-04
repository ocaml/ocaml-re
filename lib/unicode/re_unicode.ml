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
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module Cset = Cset
  module Color_map = Color_map

  module Re = struct
    module Core = Core.Make (Cset) (Color_map)
    module Replace = Replace.Make (Cset) (Color_map)
    include Core
    include Replace
    module View = View.Make (Cset) (Color_map)
    module Emacs = Emacs.Make (Cset) (Color_map)
    module Glob = Glob.Make (Cset) (Color_map)
    module Perl = Perl.Make (Cset) (Color_map)
    module Pcre = Pcre.Make (Cset) (Color_map)
    module Posix = Posix.Make (Cset) (Color_map)
    module Str = Str.Make (Cset) (Color_map)
  end
end

module Utf8 = Make (Cset.Utf8) (Color_map.Utf8)
module Utf16be = Make (Cset.Utf16be) (Color_map.Utf16be)
module Utf16le = Make (Cset.Utf16le) (Color_map.Utf16le)
module Latin1 = Make (Cset.Latin1) (Color_map.Latin1)

module Private = struct
  include Import
  module Fmt = Fmt
  module Dyn = Dyn
  module Cset = Cset.Utf8
  module Color_map = Color_map.Utf8

  module Re = struct
    module Category = Category.Make (Cset)

    module Automata = struct
      include Automata
      include Automata.Make (Cset)
    end

    module Ast = struct
      include Ast
      include Ast.Make (Cset) (Color_map)
    end

    module Compile = Compile.Make (Cset) (Color_map)
    module Core = Core.Make (Cset) (Color_map)
    module Replace = Replace.Make (Cset) (Color_map)
    include Core
    include Replace
    module View = View.Make (Cset) (Color_map)
    module Emacs = Emacs.Make (Cset) (Color_map)
    module Glob = Glob.Make (Cset) (Color_map)
    module Perl = Perl.Make (Cset) (Color_map)
    module Pcre = Pcre.Make (Cset) (Color_map)
    module Posix = Posix.Make (Cset) (Color_map)
    module Str = Str.Make (Cset) (Color_map)
  end
end

(* module Utf16be = struct
  module Re = struct
    module Core = Core.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Replace = Replace.Make (Cset.Utf16be) (Color_map.Utf16be)
    include Core
    include Replace
    module View = View.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Emacs = Emacs.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Glob = Glob.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Perl = Perl.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Pcre = Pcre.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Posix = Posix.Make (Cset.Utf16be) (Color_map.Utf16be)
    module Str = Str.Make (Cset.Utf16be) (Color_map.Utf16be)
  end
end

module Utf16le = struct
  module Re = struct
    module Core = Core.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Replace = Replace.Make (Cset.Utf16le) (Color_map.Utf16le)
    include Core
    include Replace
    module View = View.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Emacs = Emacs.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Glob = Glob.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Perl = Perl.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Pcre = Pcre.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Posix = Posix.Make (Cset.Utf16le) (Color_map.Utf16le)
    module Str = Str.Make (Cset.Utf16le) (Color_map.Utf16le)
  end
end *)
