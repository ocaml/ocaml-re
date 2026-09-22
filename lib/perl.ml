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

module Re = Core

exception Parse_error = Parse_buffer.Parse_error
exception Not_supported

let acc_digits ~base ~digits =
  if digits = [] then raise Parse_error;
  List.fold_left
    (fun acc digit ->
       if acc > (255 - digit) / base then raise Parse_error;
       (acc * base) + digit)
    0
    (List.rev digits)
;;

let char_of_int x =
  match char_of_int x with
  | x -> x
  | exception _ -> raise Parse_error
;;

type elem =
  | Char of char
  | Set of Ast.t

let char_b = Char '\008'
let char_newline = Char '\n'
let char_cr = Char '\r'
let char_tab = Char '\t'
let word_char = [ Re.alnum; Re.char '_' ]
let word = Set (Re.alt word_char)
let not_word = Set (Re.alt word_char)
let space = Set Re.space
let not_space = Set (Re.compl [ Re.space ])
let digit = Set Re.digit
let not_digit = Set (Re.compl [ Re.digit ])

module Class = struct
  let _w = Re.alt [ Re.alnum; Re.char '_' ]
  let _W = Re.compl [ Re.alnum; Re.char '_' ]
  let _S = Re.compl [ Re.space ]
  let _D = Re.compl [ Re.digit ]
  let _b = Re.alt [ Re.bow; Re.eow ]
end

let parse ~multiline ~dollar_endonly ~dotall ~ungreedy s =
  let buf = Parse_buffer.create s in
  let accept = Parse_buffer.accept buf in
  let eos () = Parse_buffer.eos buf in
  let unget () = Parse_buffer.unget buf in
  let get () = Parse_buffer.get buf in
  let greedy_mod r =
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let sequence first = function
    | [] -> first
    | rest -> Re.seq (first :: List.rev rest)
  in
  let rec regexp () =
    let first = branch () in
    if accept '|' then regexp' [ branch (); first ] else first
  and regexp' left =
    if accept '|' then regexp' (branch () :: left) else Re.alt (List.rev left)
  and branch () =
    if eos ()
    then Re.epsilon
    else (
      match get () with
      | '|' | ')' ->
        unget ();
        Re.epsilon
      | c -> branch' (piece c) [])
  and branch' first rest =
    if eos ()
    then sequence first rest
    else (
      match get () with
      | '|' | ')' ->
        unget ();
        sequence first rest
      | c -> branch' first (piece c :: rest))
  and in_brace ~f ~init =
    match accept '{' with
    | false -> None
    | true ->
      let rec loop acc =
        if accept '}'
        then acc
        else (
          let acc = f acc in
          loop acc)
      in
      Some (loop init)
  and piece c =
    let r = atom c in
    if eos ()
    then r
    else (
      match get () with
      | '*' -> greedy_mod (Re.rep r)
      | '+' -> greedy_mod (Re.rep1 r)
      | '?' -> greedy_mod (Re.opt r)
      | '{' ->
        (match Parse_buffer.integer buf with
         | Some i ->
           let j = if accept ',' then Parse_buffer.integer buf else Some i in
           if not (accept '}') then raise Parse_error;
           (match j with
            | Some j when j < i -> raise Parse_error
            | _ -> ());
           greedy_mod (Re.repn r i j)
         | None ->
           unget ();
           r)
      | _ ->
        unget ();
        r)
  and atom = function
    | '.' -> if dotall then Re.any else Re.notnl
    | '(' ->
      if accept '?'
      then
        if accept ':'
        then (
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          r)
        else if accept '#'
        then comment ()
        else if accept '<'
        then (
          let name = name () in
          let r = regexp () in
          if not (accept ')') then raise Parse_error;
          Re.group ~name r)
        else raise Parse_error
      else (
        let r = regexp () in
        if not (accept ')') then raise Parse_error;
        Re.group r)
    | '^' -> if multiline then Re.bol else Re.bos
    | '$' -> if multiline then Re.eol else if dollar_endonly then Re.leol else Re.eos
    | '[' -> if accept '^' then Re.compl (bracket []) else Re.alt (bracket [])
    | '\\' ->
      (* XXX
         - Back-references
         - \cx (control-x), \ddd
      *)
      if eos () then raise Parse_error;
      (match get () with
       | 'w' -> Class._w
       | 'W' -> Class._W
       | 's' -> Re.space
       | 'S' -> Class._S
       | 'd' -> Re.digit
       | 'D' -> Class._D
       | 'b' -> Class._b
       | 'B' -> Re.not_boundary
       | 'A' -> Re.bos
       | 'Z' -> Re.leol
       | 'z' -> Re.eos
       | 'G' -> Re.start
       | 'e' -> Re.char '\x1b'
       | 'f' -> Re.char '\x0c'
       | 'n' -> Re.char '\n'
       | 'r' -> Re.char '\r'
       | 't' -> Re.char '\t'
       | 'Q' -> quote (Buffer.create 12)
       | 'E' -> raise Parse_error
       | 'x' ->
         let c1, c2 =
           match in_brace ~init:[] ~f:(fun acc -> hexdigit () :: acc) with
           | Some [ c2; c1 ] -> c1, c2
           | Some [ c2 ] -> 0, c2
           | Some _ -> raise Parse_error
           | None ->
             let c1 = hexdigit () in
             let c2 = hexdigit () in
             c1, c2
         in
         let code = (c1 * 16) + c2 in
         Re.char (char_of_int code)
       | 'o' ->
         (match
            in_brace ~init:[] ~f:(fun acc ->
              match maybe_octaldigit () with
              | None -> raise Parse_error
              | Some p -> p :: acc)
          with
          | None -> raise Parse_error
          | Some digits -> Re.char (char_of_int (acc_digits ~base:8 ~digits)))
       | 'a' .. 'z' | 'A' .. 'Z' -> raise Parse_error
       | '0' .. '7' as n1 ->
         let n2 = maybe_octaldigit () in
         let n3 = maybe_octaldigit () in
         (match n2, n3 with
          | Some n2, Some n3 ->
            let n1 = Char.code n1 - Char.code '0' in
            Re.char (char_of_int ((n1 * (8 * 8)) + (n2 * 8) + n3))
          | _, _ -> raise Not_supported)
       | '8' .. '9' -> raise Not_supported
       | c -> Re.char c)
    | '*' | '+' | '?' | '{' -> raise Parse_error
    | c -> Re.char c
  and quote buf =
    if accept '\\'
    then (
      if eos () then raise Parse_error;
      match get () with
      | 'E' -> Re.str (Buffer.contents buf)
      | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        quote buf)
    else (
      if eos () then raise Parse_error;
      Buffer.add_char buf (get ());
      quote buf)
  and hexdigit () =
    if eos () then raise Parse_error;
    match get () with
    | '0' .. '9' as d -> Char.code d - Char.code '0'
    | 'a' .. 'f' as d -> Char.code d - Char.code 'a' + 10
    | 'A' .. 'F' as d -> Char.code d - Char.code 'A' + 10
    | _ -> raise Parse_error
  and maybe_octaldigit () =
    if eos ()
    then None
    else (
      match get () with
      | '0' .. '7' as d -> Some (Char.code d - Char.code '0')
      | _ -> None)
  and name () =
    let start = Parse_buffer.position buf in
    let rec find_end pos =
      if pos = String.length s then raise Parse_error;
      match s.[pos] with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' -> find_end (pos + 1)
      | '0' .. '9' when pos > start -> find_end (pos + 1)
      | '>' when pos > start -> pos
      | _ -> raise Parse_error
    in
    let stop = find_end start in
    Parse_buffer.advance buf (stop + 1 - start);
    String.sub s start (stop - start)
  and bracket s =
    if s <> [] && accept ']'
    then s
    else (
      match char () with
      | Set st -> bracket (st :: s)
      | Char c ->
        if accept '-'
        then
          if accept ']'
          then Re.char c :: Re.char '-' :: s
          else
            bracket
              (match char () with
               | Char c' -> Re.rg c c' :: s
               | Set st' -> Re.char c :: Re.char '-' :: st' :: s)
        else bracket (Re.char c :: s))
  and char () =
    if eos () then raise Parse_error;
    let c = get () in
    if c = '['
    then (
      if accept '=' then raise Not_supported;
      match Posix_class.parse buf with
      | Some set -> Set set
      | None ->
        if accept '.'
        then (
          if eos () then raise Parse_error;
          let c = get () in
          if not (accept '.') then raise Not_supported;
          if not (accept ']') then raise Parse_error;
          Char c)
        else Char c)
    else if c = '\\'
    then (
      if eos () then raise Parse_error;
      let c = get () in
      (* XXX
         \127, ...
      *)
      match c with
      | 'b' -> char_b
      | 'n' -> char_newline (*XXX*)
      | 'r' -> char_cr (*XXX*)
      | 't' -> char_tab (*XXX*)
      | 'w' -> word
      | 'W' -> not_word
      | 's' -> space
      | 'S' -> not_space
      | 'd' -> digit
      | 'D' -> not_digit
      | 'a' .. 'z' | 'A' .. 'Z' -> raise Parse_error
      | '0' .. '9' -> raise Not_supported
      | _ -> Char c)
    else Char c
  and comment () =
    if eos () then raise Parse_error;
    if accept ')'
    then Re.epsilon
    else (
      Parse_buffer.junk buf;
      comment ())
  in
  let res = regexp () in
  if not (eos ()) then raise Parse_error;
  res
;;

type opt =
  [ `Ungreedy
  | `Dotall
  | `Dollar_endonly
  | `Multiline
  | `Anchored
  | `Caseless
  ]

let re ?(opts = []) s =
  let r =
    parse
      ~multiline:(List.memq `Multiline opts)
      ~dollar_endonly:(List.memq `Dollar_endonly opts)
      ~dotall:(List.memq `Dotall opts)
      ~ungreedy:(List.memq `Ungreedy opts)
      s
  in
  let r = if List.memq `Anchored opts then Re.seq [ Re.start; r ] else r in
  let r = if List.memq `Caseless opts then Re.no_case r else r in
  r
;;

let compile = Re.compile
let compile_pat ?(opts = []) s = compile (re ~opts s)

let re_result ?opts s =
  match re ?opts s with
  | s -> Ok s
  | exception Not_supported -> Error `Not_supported
  | exception Parse_error -> Error `Parse_error
;;
