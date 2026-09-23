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

let char_of_int x =
  match char_of_int x with
  | x -> x
  | exception _ -> raise Parse_error
;;

type elem =
  | Char of char
  | Set of Ast.t

module Class = struct
  let _w = Re.alt [ Re.alnum; Re.char '_' ]
  let _W = Re.compl [ Re.alnum; Re.char '_' ]
  let _S = Re.compl [ Re.space ]
  let _D = Re.compl [ Re.digit ]
  let _b = Re.alt [ Re.bow; Re.eow ]
end

let character_type =
  let horizontal = Re.set "\t \160" in
  let vertical = Re.set "\n\011\012\r\133" in
  let not_horizontal = Re.compl [ horizontal ] in
  let not_vertical = Re.compl [ vertical ] in
  function
  | 'w' -> Some Class._w
  | 'W' -> Some Class._W
  | 's' -> Some Re.space
  | 'S' -> Some Class._S
  | 'd' -> Some Re.digit
  | 'D' -> Some Class._D
  | 'h' -> Some horizontal
  | 'H' -> Some not_horizontal
  | 'v' -> Some vertical
  | 'V' -> Some not_vertical
  | _ -> None
;;

let is_literal = function
  | '.' | '(' | ')' | '|' | '^' | '$' | '[' | '\\' | '*' | '+' | '?' | '{' -> false
  | _ -> true
;;

let parse ~multiline ~dollar_endonly ~dotall ~ungreedy s =
  let buf = Parse_buffer.create s in
  let quoted = ref false in
  let accept c = (not !quoted) && Parse_buffer.accept buf c in
  let eos () = Parse_buffer.eos buf in
  let unget () = Parse_buffer.unget buf in
  let get () = Parse_buffer.get buf in
  (* Quoting changes tokenization; it does not introduce an atom or group.
     In particular, a following quantifier applies to the last quoted byte,
     and an empty quote must not separate a quantifier from its operand. *)
  let rec skip_quote_markers () =
    if Parse_buffer.accept_s buf "\\E"
    then (
      quoted := false;
      skip_quote_markers ())
    else if (not !quoted) && Parse_buffer.accept_s buf "\\Q"
    then (
      quoted := true;
      skip_quote_markers ())
  in
  let rec skip_comment () =
    if eos () then raise Parse_error;
    if get () <> ')' then skip_comment ()
  in
  let rec skip_ignored () =
    skip_quote_markers ();
    if (not !quoted) && Parse_buffer.accept_s buf "(?#"
    then (
      skip_comment ();
      skip_ignored ())
  in
  let captures = ref 0 in
  let maybe_digit base =
    if eos ()
    then None
    else (
      let value =
        match get () with
        | '0' .. '9' as c -> Char.code c - Char.code '0'
        | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
        | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
        | _ -> base
      in
      if value < base
      then Some value
      else (
        unget ();
        None))
  in
  let add_digit base value digit =
    (* Check before accumulating, including in arbitrarily long braced escapes. *)
    if value > (255 - digit) / base then raise Parse_error;
    (value * base) + digit
  in
  let rec digits base remaining value =
    if remaining = 0
    then value
    else (
      match maybe_digit base with
      | None -> value
      | Some digit -> digits base (remaining - 1) (add_digit base value digit))
  in
  let rec brace_space () = if accept ' ' || accept '\t' then brace_space () in
  let braced_code base =
    brace_space ();
    let rec loop value =
      match maybe_digit base with
      | Some digit -> loop (add_digit base value digit)
      | None ->
        brace_space ();
        if not (accept '}') then raise Parse_error;
        value
    in
    match maybe_digit base with
    | None -> raise Parse_error
    | Some digit -> loop digit
  in
  let numeric_escape ~in_class first =
    if (not in_class) && first <> '0'
    then (
      (* PCRE interprets a nonzero decimal escape as a backreference when it
         is a single digit, starts with 8/9, or names an already opened group.
         Inspect without consuming; unsupported references must never silently
         become octal characters. Saturation avoids overflow on long escapes. *)
      if first >= '8' then raise Not_supported;
      let limit = max 10 (!captures + 1) in
      let rec decimal count value =
        match maybe_digit 10 with
        | Some digit -> decimal (count + 1) (min limit ((value * 10) + digit))
        | None ->
          for _ = 1 to count do
            unget ()
          done;
          value
      in
      let value = decimal 0 (Char.code first - Char.code '0') in
      if value < 10 || value <= !captures then raise Not_supported);
    if first >= '8'
    then first
    else char_of_int (digits 8 2 (Char.code first - Char.code '0'))
  in
  let byte_escape ~in_class = function
    | 'a' -> '\007'
    | 'b' when in_class -> '\008'
    | 'e' -> '\027'
    | 'f' -> '\012'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'c' ->
      if eos () then raise Parse_error;
      let c = get () in
      if c < ' ' || c > '~' then raise Parse_error;
      Char.chr (Char.code (Char.uppercase_ascii c) lxor 0x40)
    | 'x' ->
      let value =
        if accept '{'
        then braced_code 16
        else (
          match maybe_digit 16 with
          | None -> raise Parse_error
          | Some digit -> digits 16 1 digit)
      in
      char_of_int value
    | 'o' ->
      if not (accept '{') then raise Parse_error;
      char_of_int (braced_code 8)
    | '0' .. '9' as c -> numeric_escape ~in_class c
    | 'a' .. 'z' | 'A' .. 'Z' -> raise Parse_error
    | c -> c
  in
  let greedy_mod r =
    skip_ignored ();
    let gr = accept '?' in
    let gr = if ungreedy then not gr else gr in
    if gr then Re.non_greedy r else Re.greedy r
  in
  let length = String.length s in
  let rec literal_end i =
    if i < length && is_literal s.[i] then literal_end (i + 1) else i
  in
  (* Tokens that do not separate a quantifier from the preceding byte: stray
     \E, empty quotes, and inline comments. *)
  let rec skippable_markers i =
    if i + 1 < length && s.[i] = '\\'
    then (
      match s.[i + 1] with
      | 'E' -> skippable_markers (i + 2)
      | 'Q' ->
        if i + 3 < length && s.[i + 2] = '\\' && s.[i + 3] = 'E'
        then skippable_markers (i + 4)
        else -1
      | _ -> i)
    else if i + 2 < length && s.[i] = '(' && s.[i + 1] = '?' && s.[i + 2] = '#'
    then (
      match String.index_from s (i + 3) ')' with
      | exception Not_found -> -1
      | stop -> skippable_markers (stop + 1))
    else i
  in
  let literal_stop start =
    let stop = literal_end (start + 1) in
    if stop < length
    then (
      let next = skippable_markers stop in
      if next >= 0 && next < length
      then (
        match s.[next] with
        | '*' | '+' | '?' | '{' -> stop - 1
        | _ -> stop)
      else stop)
    else stop
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
  and item c =
    if (not !quoted) && is_literal c
    then (
      let start = Parse_buffer.position buf - 1 in
      let stop = literal_stop start in
      if stop = start
      then piece c
      else (
        Parse_buffer.advance buf (stop - start - 1);
        if stop = start + 1 then Re.char c else Re.str (String.sub s start (stop - start))))
    else piece c
  and branch () =
    skip_ignored ();
    if eos ()
    then Re.epsilon
    else (
      match get () with
      | ('|' | ')') when not !quoted ->
        unget ();
        Re.epsilon
      | c -> branch' (item c) [])
  and branch' first rest =
    skip_ignored ();
    if eos ()
    then sequence first rest
    else (
      match get () with
      | ('|' | ')') when not !quoted ->
        unget ();
        sequence first rest
      | c -> branch' first (item c :: rest))
  and piece c =
    let r = atom c in
    skip_ignored ();
    if eos () || !quoted
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
  and atom c =
    if !quoted
    then Re.char c
    else (
      match c with
      | '.' -> if dotall then Re.any else Re.notnl
      | '(' ->
        if accept '?'
        then
          if accept ':'
          then (
            let r = regexp () in
            if not (accept ')') then raise Parse_error;
            r)
          else if accept '<'
          then named_group '>'
          else if accept '\''
          then named_group '\''
          else if accept 'P'
          then (
            if not (accept '<') then raise Parse_error;
            named_group '>')
          else raise Parse_error
        else group ()
      | '^' -> if multiline then Re.bol else Re.bos
      | '$' -> if multiline then Re.eol else if dollar_endonly then Re.eos else Re.leol
      | '[' ->
        if Parse_buffer.accept_s buf "[:<:]]"
        then Re.bow
        else if Parse_buffer.accept_s buf "[:>:]]"
        then Re.eow
        else (
          skip_quote_markers ();
          if accept '^' then Re.compl (bracket []) else Re.alt (bracket []))
      | '\\' ->
        if eos () then raise Parse_error;
        (match get () with
         | 'C' -> Re.any
         | 'N' -> Re.notnl
         | 'b' -> Class._b
         | 'B' -> Re.not_boundary
         | 'A' -> Re.bos
         | 'Z' -> Re.leol
         | 'z' -> Re.eos
         | 'G' -> Re.start
         | c ->
           (match character_type c with
            | Some set -> set
            | None -> Re.char (byte_escape ~in_class:false c)))
      | '*' | '+' | '?' | '{' -> raise Parse_error
      | c -> Re.char c)
  and group ?name () =
    incr captures;
    let r = regexp () in
    if not (accept ')') then raise Parse_error;
    Re.group ?name r
  and named_group delimiter =
    let name = name delimiter in
    group ~name ()
  and name delimiter =
    let start = Parse_buffer.position buf in
    let rec find_end pos =
      if pos = String.length s then raise Parse_error;
      match s.[pos] with
      | '_' | 'a' .. 'z' | 'A' .. 'Z' -> find_end (pos + 1)
      | '0' .. '9' when pos > start -> find_end (pos + 1)
      | c when c = delimiter && pos > start -> pos
      | _ -> raise Parse_error
    in
    let stop = find_end start in
    Parse_buffer.advance buf (stop + 1 - start);
    String.sub s start (stop - start)
  and bracket s =
    skip_quote_markers ();
    if s <> [] && accept ']'
    then s
    else (
      match char () with
      | Set st -> bracket (st :: s)
      | Char c ->
        skip_quote_markers ();
        if accept '-'
        then (
          skip_quote_markers ();
          if accept ']'
          then Re.char c :: Re.char '-' :: s
          else
            bracket
              (match char () with
               | Char c' -> Re.rg c c' :: s
               | Set st' -> Re.char c :: Re.char '-' :: st' :: s))
        else bracket (Re.char c :: s))
  and char () =
    skip_quote_markers ();
    if eos () then raise Parse_error;
    let c = get () in
    if !quoted
    then Char c
    else if c = '['
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
      match character_type c with
      | Some set -> Set set
      | None -> Char (byte_escape ~in_class:true c))
    else Char c
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
