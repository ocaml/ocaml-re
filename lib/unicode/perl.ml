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

(* TODO: https://www.pcre.org/original/doc/html/pcrepattern.html
   add in module Pcre:
   - \p{}: a character with the xx property
   - \P{}  a character without the xx property
   - \X
   - Xan   Any alphanumeric character
   - Xps   Any POSIX space character
   - Xsp   Any Perl space character
   - Xwd   Any Perl "word" characte
*)

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

type opt =
  [ `Ungreedy | `Dotall | `Dollar_endonly | `Multiline | `Anchored | `Caseless ]

module type T = sig
  type core
  type re

  (** Parsing of a Perl-style regular expression *)
  val re : ?opts:opt list -> string -> core

  val re_result :
    ?opts:opt list -> string -> (core, [ `Not_supported | `Parse_error ]) result

  (** (Same as [Re.compile]) *)
  val compile : core -> re

  (** Regular expression compilation *)
  val compile_pat : ?opts:opt list -> string -> re
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module CodePage = Cset.CodePage
  module Categories = CodePage.Categories
  module Posix_class = Posix_class.Make (Cset) (Color_map)
  module Parse_buffer = Posix_class.Parse_buffer
  module Re = Core.Make (Cset) (Color_map)

  module Ast = struct
    include Ast
    include Ast.Make (Cset) (Color_map)
  end

  let ( !! ) = CodePage.of_char
  let ( !^ ) = fun x -> CodePage.from_letter @@ CodePage.of_char x

  let acc_digits =
    let rec loop base digits acc i =
      match digits with
      | [] -> acc
      | d :: digits ->
        let acc = acc + (d * i) in
        let i = i * i in
        loop base digits acc i
    in
    fun ~base ~digits -> loop base digits 0 1

  let letter_of_int x =
    try CodePage.(of_int x |> to_letter)
    with _exn ->
      raise Parse_error

  type elem = Letter of Cset.letter | Set of Re.t

  let letter_b = Letter !!'\008'
  let letter_newline = Letter !!'\n'
  let letter_cr = Letter !!'\r'
  let letter_tab = Letter !!'\t'
  let word_letter = [ Re.alnum; Re.letter !!'_' ]
  let word = Set (Re.alt word_letter)
  let not_word = Set (Re.alt word_letter)
  let space = Set Re.space
  let not_space = Set (Re.compl [ Re.space ])
  let digit = Set Re.digit
  let not_digit = Set (Re.compl [ Re.digit ])
  let xdigit_lowercase = Cset.inter Cset.xdigit Cset.clower
  let xdigit_uppercase = Cset.inter Cset.xdigit Cset.cupper

  let cname =
    Cset.union Cset.calpha @@ Cset.single CodePage.(from_letter @@ of_char '_')

  let parse ~multiline ~dollar_endonly ~dotall ~ungreedy s =
    let buf = Parse_buffer.create s in
    let accept = Parse_buffer.accept buf in
    let eos () = Parse_buffer.eos buf in
    let test c = Parse_buffer.test buf c in
    let unget () = Parse_buffer.unget buf in
    let get () = Parse_buffer.get buf in
    let greedy_mod r =
      let gr = accept !!'?' in
      let gr = if ungreedy then not gr else gr in
      if gr then Re.non_greedy r else Re.greedy r
    in
    let rec regexp () = regexp' [ branch () ]
    and regexp' left =
      if accept !!'|' then regexp' (branch () :: left)
      else Re.alt (List.rev left)
    and branch () = branch' []
    and branch' left =
      if eos () || (fst @@ test !!'|') || (fst @@ test !!')') then
        Re.seq (List.rev left)
      else branch' (piece () :: left)
    and in_brace ~f ~init =
      match accept !!'{' with
      | false -> None
      | true ->
        let rec loop acc =
          if accept !!'}' then acc
          else
            let acc = f acc in
            loop acc
        in
        Some (loop init)
    and piece () =
      let r = atom () in
      if accept !!'*' then greedy_mod (Re.rep r)
      else if accept !!'+' then greedy_mod (Re.rep1 r)
      else if accept !!'?' then greedy_mod (Re.opt r)
      else if accept !!'{' then (
        match Parse_buffer.integer buf with
        | Some i ->
          let j = if accept !!',' then Parse_buffer.integer buf else Some i in
          if not (accept !!'}') then raise Parse_error;
          (match j with Some j when j < i -> raise Parse_error | _ -> ());
          greedy_mod (Re.repn r i j)
        | None ->
          unget ();
          r)
      else r
    and atom () =
      if accept !!'.' then if dotall then Re.any else Re.notnl
      else if accept !!'(' then (
        if accept !!'?' then
          if accept !!':' then (
            let r = regexp () in
            if not (accept !!')') then raise Parse_error;
            r)
          else if accept !!'#' then comment ()
          else if accept !!'<' then (
            let name = name () in
            let r = regexp () in
            if not (accept !!')') then raise Parse_error;
            Re.group ~name r)
          else raise Parse_error
        else
          let r = regexp () in
          if not (accept !!')') then raise Parse_error;
          Re.group r)
      else if accept !!'^' then if multiline then Re.bol else Re.bos
      else if accept !!'$' then
        if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
      else if accept !!'[' then
        if accept !!'^' then Re.compl (bracket []) else Re.alt (bracket [])
      else if accept !!'\\' then (
        (* XXX
         - Back-references
         - \cx (control-x), \ddd
      *)
        if eos () then raise Parse_error;
        match CodePage.from_letter (get ()) with
        | cp when CodePage.equal cp !^'w' ->
          Re.alt [ Re.alnum; Re.letter !!'_' ]
        | cp when CodePage.equal cp !^'W' ->
          Re.compl [ Re.alnum; Re.letter !!'_' ]
        | cp when CodePage.equal cp !^'s' -> Re.space
        | cp when CodePage.equal cp !^'S' -> Re.compl [ Re.space ]
        | cp when CodePage.equal cp !^'d' -> Re.digit
        | cp when CodePage.equal cp !^'D' -> Re.compl [ Re.digit ]
        | cp when CodePage.equal cp !^'b' -> Re.alt [ Re.bow; Re.eow ]
        | cp when CodePage.equal cp !^'B' -> Re.not_boundary
        | cp when CodePage.equal cp !^'A' -> Re.bos
        | cp when CodePage.equal cp !^'Z' -> Re.leol
        | cp when CodePage.equal cp !^'z' -> Re.eos
        | cp when CodePage.equal cp !^'G' -> Re.start
        | cp when CodePage.equal cp !^'e' -> Re.letter !!'\x1b'
        | cp when CodePage.equal cp !^'f' -> Re.letter !!'\x0c'
        | cp when CodePage.equal cp !^'n' -> Re.letter !!'\n'
        | cp when CodePage.equal cp !^'r' -> Re.letter !!'\r'
        | cp when CodePage.equal cp !^'t' -> Re.letter !!'\t'
        | cp when CodePage.equal cp !^'Q' -> quote (Buffer.create 12)
        | cp when CodePage.equal cp !^'E' -> raise Parse_error
        | cp when CodePage.equal cp !^'x' ->
          let c1, c2 =
            match in_brace ~init:[] ~f:(fun acc -> hexdigit () :: acc) with
            | Some [ c1; c2 ] ->
              (c1, c2)
            | Some [ c2 ] ->
              (0, c2)
            | Some _l ->
              raise Parse_error
            | None ->
              let c1 = hexdigit () in
              let c2 = hexdigit () in
              (c1, c2)
          in
          let code = (c1 * 16) + c2 in
          let t = Re.letter (letter_of_int code) in
          t
        | cp when CodePage.equal cp !^'o' -> (
          match
            in_brace ~init:[] ~f:(fun acc ->
              match maybe_octaldigit () with
              | None -> raise Parse_error
              | Some p -> p :: acc)
          with
          | None -> raise Parse_error
          | Some digits ->
            Re.letter (letter_of_int (acc_digits ~base:8 ~digits)))
        | cp when Cset.mem cp Cset.calpha -> raise Parse_error
        | cp when Cset.mem cp Cset.cdigit ->
          let n1 =
            CodePage.to_int cp - CodePage.(to_int @@ from_letter @@ of_char '0')
          in
          if n1 < 8 then
            let n2 = maybe_octaldigit () in
            let n3 = maybe_octaldigit () in
            match (n2, n3) with
            | Some n2, Some n3 ->
              Re.letter (letter_of_int ((n1 * (8 * 8)) + (n2 * 8) + n3))
            | _, _ -> raise Not_supported
          else raise Not_supported
        | c -> Re.letter @@ CodePage.to_letter c)
      else (
        if eos () then raise Parse_error;
        match CodePage.from_letter (get ()) with
        | cp when CodePage.equal cp !^'*' -> raise Parse_error
        | cp when CodePage.equal cp !^'+' -> raise Parse_error
        | cp when CodePage.equal cp !^'?' -> raise Parse_error
        | cp when CodePage.equal cp !^')' -> raise Parse_error
        | cp when CodePage.equal cp !^'\\' -> raise Parse_error
        | c -> Re.letter @@ CodePage.to_letter c)
    and quote buf =
      if accept !!'\\' then (
        if eos () then raise Parse_error;
        match CodePage.from_letter (get ()) with
        | cp when CodePage.equal cp !^'E' ->
          let qs = Buffer.contents buf in
          Re.str qs
        | cp ->
          Cset.Codec.add buf !!'\\';
          Cset.Codec.add buf @@ CodePage.to_letter cp;
          quote buf)
      else (
        if eos () then raise Parse_error;
        Cset.Codec.add buf @@ get ();
        quote buf)
    and hexdigit () =
      if eos () then raise Parse_error;
      match CodePage.from_letter (get ()) with
      | cp when Cset.mem cp Cset.cdigit ->
        CodePage.to_int cp - CodePage.(to_int @@ from_letter @@ of_char '0')
      | cp when Cset.mem cp xdigit_lowercase ->
        CodePage.to_int cp
        - CodePage.(to_int @@ from_letter @@ of_char 'a')
        + 10
      | cp when Cset.mem cp xdigit_uppercase ->
        CodePage.to_int cp
        - CodePage.(to_int @@ from_letter @@ of_char 'A')
        + 10
      | _ -> raise Parse_error
    and maybe_octaldigit () =
      if eos () then None
      else
        match CodePage.from_letter (get ()) with
        | cp when Cset.mem cp Cset.cdigit ->
          let n =
            CodePage.to_int cp - CodePage.(to_int @@ from_letter @@ of_char '0')
          in
          if n < 8 then Some n else None
        | _ -> None
    and name () =
      if eos () then raise Parse_error
      else
        match CodePage.from_letter @@ get () with
        | cp when Cset.mem cp cname ->
          let b = Buffer.create 32 in
          Cset.Codec.add b @@ CodePage.to_letter cp;
          name' b
        | _ -> raise Parse_error
    and name' b =
      if eos () then raise Parse_error
      else
        match CodePage.from_letter @@ get () with
        | cp when Cset.mem cp Cset.cword ->
          Cset.Codec.add b @@ CodePage.to_letter cp;
          name' b
        | cp when CodePage.equal cp !^'>' -> Buffer.contents b
        | _ -> raise Parse_error
    and bracket s =
      if s <> [] && accept !!']' then s
      else
        match letter () with
        | Set st -> bracket (st :: s)
        | Letter l ->
          if accept !!'-' then
            if accept !!']' then Re.letter l :: Re.letter !!'-' :: s
            else
              bracket
                (match letter () with
                | Letter l' -> Re.rg l l' :: s
                | Set st' -> Re.letter l :: Re.letter !!'-' :: st' :: s)
          else bracket (Re.letter l :: s)
    and letter () =
      if eos () then raise Parse_error;
      let l = get () in
      if CodePage.equal (CodePage.from_letter l) !^'[' then (
        if accept !!'=' then raise Not_supported;
        match Posix_class.parse buf with
        | Some set -> Set set
        | None ->
          if accept !!'.' then (
            if eos () then raise Parse_error;
            let l' = get () in
            if not (accept !!'.') then raise Not_supported;
            if not (accept !!']') then raise Parse_error;
            Letter l')
          else Letter l)
      else if CodePage.equal (CodePage.from_letter l) !^'\\' then (
        if eos () then raise Parse_error;
        let l' = get () in
        (* XXX
         \127, ...
      *)
        match CodePage.from_letter l' with
        | cp when CodePage.equal cp !^'b' -> letter_b
        | cp when CodePage.equal cp !^'n' -> letter_newline (*XXX*)
        | cp when CodePage.equal cp !^'r' -> letter_cr (*XXX*)
        | cp when CodePage.equal cp !^'t' -> letter_tab (*XXX*)
        | cp when CodePage.equal cp !^'w' -> word
        | cp when CodePage.equal cp !^'W' -> not_word
        | cp when CodePage.equal cp !^'s' -> space
        | cp when CodePage.equal cp !^'S' -> not_space
        | cp when CodePage.equal cp !^'d' -> digit
        | cp when CodePage.equal cp !^'D' -> not_digit
        | cp when Cset.mem cp Cset.calpha -> raise Parse_error
        | cp when Cset.mem cp Cset.cdigit -> raise Not_supported
        | _ -> Letter l')
      else Letter l
    and comment () =
      if eos () then raise Parse_error;
      if accept !!')' then Re.epsilon
      else (
        Parse_buffer.junk buf;
        comment ())
    in
    let res = regexp () in
    if not (eos ()) then raise Parse_error;
    res

  let re ?(opts = []) s =
    let r =
      parse
        ~multiline:(List.memq `Multiline opts)
        ~dollar_endonly:(List.memq `Dollar_endonly opts)
        ~dotall:(List.memq `Dotall opts) ~ungreedy:(List.memq `Ungreedy opts) s
    in
    let r = if List.memq `Anchored opts then Re.seq [ Re.start; r ] else r in
    let r = if List.memq `Caseless opts then Re.no_case r else r in
    r

  let compile = Re.compile
  let compile_pat ?(opts = []) s = compile (re ~opts s)

  let re_result ?opts s =
    match re ?opts s with
    | s -> Ok s
    | exception Not_supported -> Error `Not_supported
    | exception Parse_error -> Error `Parse_error
end
