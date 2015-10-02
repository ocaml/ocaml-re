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

(** Shell-style regular expressions *)

exception Parse_error

val glob :
  ?anchored:bool ->
  ?period:bool ->
  ?expand_braces:bool ->
  string ->
  Re.t
(** Implements the semantics of shells patterns. The returned regular
    expression is unanchored by default.

    [anchored] controls whether the regular expression will only match entire
    strings.

    [period] controls whether a dot at the beginning of a file
    name must be explicitly matched.

    If [expand_braces] is true, braced sets will expand into multiple globs,
    e.g. a\{x,y\}b\{1,2\} matches axb1, axb2, ayb1, ayb2.

    Character '/' must be explicitly matched.
    Character '*' matches any sequence of characters and character
    '?' matches a single character, provided these restrictions are
    satisfied.
    A sequence '[...]' matches any of the enclosed characters.
    A backslash escapes the following character.
*)

val glob' : ?anchored:bool -> bool -> string -> Re.t
(** Same, but allows to choose whether dots at the beginning of a
    file name need to be explicitly matched (true) or not (false)

    @deprecated Use [glob] with the optional [period] parameter.
*)

val globx : ?anchored:bool -> string -> Re.t
(** This version of [glob] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true].
*)

val globx' : ?anchored:bool -> bool -> string -> Re.t
(** This version of [glob'] also recognizes the pattern \{..,..\}

    @deprecated Prefer [glob ~expand_braces:true ?period].
*)
