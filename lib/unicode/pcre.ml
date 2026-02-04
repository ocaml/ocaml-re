exception Parse_error = Perl.Parse_error
exception Not_supported = Perl.Not_supported

type flag = [ `CASELESS | `MULTILINE | `ANCHORED | `DOTALL ]

type split_result =
  | Text of string
  | Delim of string
  | Group of int * string
  | NoGroup

module type T = sig
  type core
  type re
  type groups

  (** [re ~flags s] creates the regexp [s] using the pcre syntax. *)
  val re : ?flags:flag list -> string -> core

  val re_result :
    ?flags:flag list ->
    string ->
    (core, [ `Not_supported | `Parse_error ]) result

  (** [re ~flags s] compiles the regexp [s] using the pcre syntax. *)
  val regexp : ?flags:flag list -> string -> re

  (** [extract ~rex s] executes [rex] on [s] and returns the matching groups. *)
  val extract : rex:re -> string -> string array

  (** Equivalent to {!Core.exec}. *)
  val exec : rex:re -> ?pos:int -> string -> groups

  (** Equivalent to {!Core.Group.get}. *)
  val get_substring : groups -> int -> string

  (** Return the names of named groups. *)
  val names : re -> string array

  (** Return the first matched named group, or raise [Not_found]. Prefer to use
      the non-raising version [get_named_substring_opt] *)
  val get_named_substring : re -> string -> groups -> string

  (** Return the first matched named group, or raise [Not_found]. *)
  val get_named_substring_opt : re -> string -> groups -> string option

  (** Equivalent to {!Core.Group.offset}. *)
  val get_substring_ofs : groups -> int -> int * int

  (** Equivalent to {!Core.execp}. *)
  val pmatch : rex:re -> string -> bool

  val substitute : rex:re -> subst:(string -> string) -> string -> string
  val full_split : ?max:int -> rex:re -> string -> split_result list
  val split : rex:re -> string -> string list
  val quote : string -> string

  (** {2 Deprecated} *)

  type substrings = Group.t
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module Re = Core.Make (Cset) (Color_map)

  type core = Re.t
  type re = Re.re
  type groups = Re.Group.t

  module Perl = Perl.Make (Cset) (Color_map)

  let re ?(flags = []) pat =
    let opts =
      List.map
        (function
          | `CASELESS -> `Caseless
          | `MULTILINE -> `Multiline
          | `ANCHORED -> `Anchored
          | `DOTALL -> `Dotall)
        flags
    in
    Perl.re ~opts pat

  let re_result ?flags s =
    match re ?flags s with
    | s -> Ok s
    | exception Not_supported -> Error `Not_supported
    | exception Parse_error -> Error `Parse_error

  let regexp ?flags pat = Re.compile (re ?flags pat)
  let extract ~rex s = Re.Group.all (Re.exec rex s)
  let exec ~rex ?pos s = Re.exec rex ?pos s
  let names rex = Re.group_names rex |> List.map fst |> Array.of_list

  let get_named_substring_opt rex name s =
    let rec loop = function
      | [] -> None
      | (n, i) :: rem when n = name -> (
        match Re.Group.get_opt s i with None -> loop rem | Some _ as s -> s)
      | _ :: rem -> loop rem
    in
    loop (Re.group_names rex)

  let get_substring_ofs s i = Re.Group.offset s i
  let pmatch ~rex s = Re.execp rex s

  let substitute ~rex ~subst str =
    let b = Buffer.create 1024 in
    let rec loop pos on_match =
      if Re.execp ~pos rex str then (
        let ss = Re.exec ~pos rex str in
        let start, fin = Re.Group.offset ss 0 in
        if on_match && start = pos && start = fin then (
          if
            (* Empty match following a match *)
            pos < String.length str
          then (
            Buffer.add_char b str.[pos];
            loop (pos + 1) false))
        else
          let pat = Re.Group.get ss 0 in
          Buffer.add_substring b str pos (start - pos);
          Buffer.add_string b (subst pat);
          if start = fin then (
            if
              (* Manually advance by one after an empty match *)
              fin < String.length str
            then (
              Buffer.add_char b str.[fin];
              loop (fin + 1) false))
          else loop fin true)
      else Buffer.add_substring b str pos (String.length str - pos)
    in
    loop 0 false;
    Buffer.contents b

  let split ~rex s =
    let rec split accu start =
      if start = String.length s then accu
      else
        match
          let g = Re.exec rex s ~pos:start in
          if Group.stop g 0 = start then Re.exec rex s ~pos:(start + 1) else g
        with
        | exception Not_found ->
          String.sub s start (String.length s - start) :: accu
        | g ->
          let next = Group.stop g 0 in
          split (String.sub s start (Group.start g 0 - start) :: accu) next
    in
    match Re.exec rex s ~pos:0 with
    | g ->
      List.rev
        (if Group.start g 0 = 0 then split [] (Group.stop g 0)
         else split [ String.sub s 0 (Group.start g 0) ] (Group.stop g 0))
    | exception Not_found -> if s = "" then [] else [ s ]

  (* From PCRE *)
  let string_unsafe_sub s ofs len =
    let r = Bytes.create len in
    Bytes.unsafe_blit s ofs r 0 len;
    Bytes.unsafe_to_string r

  let quote s =
    let len = String.length s in
    let buf = Bytes.create (len lsl 1) in
    let pos = ref 0 in
    for i = 0 to len - 1 do
      match String.unsafe_get s i with
      | ('\\' | '^' | '$' | '.' | '[' | '|' | '(' | ')' | '?' | '*' | '+' | '{')
        as c ->
        Bytes.unsafe_set buf !pos '\\';
        incr pos;
        Bytes.unsafe_set buf !pos c;
        incr pos
      | c ->
        Bytes.unsafe_set buf !pos c;
        incr pos
    done;
    string_unsafe_sub buf 0 !pos

  let full_split ?(max = 0) ~rex s =
    if String.length s = 0 then []
    else if max = 1 then [ Text s ]
    else
      let results = Re.split_full rex s in
      let matches =
        List.map
          (function
            | `Text s -> [ Text s ]
            | `Delim d ->
              let matches = Re.Group.all_offset d in
              let delim = Re.Group.get d 0 in
              Delim delim
              ::
              (let l = ref [] in
               for i = 1 to Array.length matches - 1 do
                 l :=
                   (if matches.(i) = (-1, -1) then NoGroup
                    else Group (i, Re.Group.get d i))
                   :: !l
               done;
               List.rev !l))
          results
      in
      List.concat matches

  type substrings = Group.t

  let get_substring s i = Re.Group.get s i

  let get_named_substring rex name s =
    match get_named_substring_opt rex name s with
    | None -> raise Not_found
    | Some s -> s
end
