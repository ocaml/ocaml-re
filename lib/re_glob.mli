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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

exception Parse_error

val glob : string -> Re.t
   (* Implements the semantics of shells patterns. Not that the returned
      regular expression is unanchored, so you will probably want to combine
      it with [Re.bos] and [Re.eos].

      Character '/' must be explicitely matched.  A dot at the
      beginning of a file name must be explicitely matched as well.
      Character '*' matches any sequence of characters and character
      '?' matches a single character, provided these restrictions are
      satisfied,
      A sequence '[...]' matches any of the enclosed characters.
      A backslash escapes the following character. *)

val glob' : bool -> string -> Re.t
   (* Same, but allows to choose whether dots at the beginning of a
      file name need to be explicitly matched (true) or not (false) *)

val globx : string -> Re.t
val globx' : bool -> string -> Re.t
    (* These two functions also recognize the pattern {..,..} *)
