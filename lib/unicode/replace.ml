module type T = sig
  type re

  (** [replace ~all re ~f s] iterates on [s], and replaces every occurrence of
      [re] with [f substring] where [substring] is the current match. If
      [all = false], then only the first occurrence of [re] is replaced. *)
  val replace :
    ?pos:int (** Default: 0 *) ->
    ?len:int ->
    ?all:bool (** Default: true. Otherwise only replace first occurrence *) ->
    re (** matched groups *) ->
    f:(Group.t -> string) (** how to replace *) ->
    string (** string to replace in *) ->
    string

  (** [replace_string ~all re ~by s] iterates on [s], and replaces every
      occurrence of [re] with [by]. If [all = false], then only the first
      occurrence of [re] is replaced.

      {5 Examples:}
      {[
        # let regex = Re.compile (Re.char ',');;
        val regex : re = <abstr>

        # Re.replace_string regex ~by:";" "[1,2,3,4,5,6,7]";;
        - : string = "[1;2;3;4;5;6;7]"

        # Re.replace_string regex ~all:false ~by:";" "[1,2,3,4,5,6,7]";;
        - : string = "[1;2,3,4,5,6,7]"
      ]} *)
  val replace_string :
    ?pos:int (** Default: 0 *) ->
    ?len:int ->
    ?all:bool (** Default: true. Otherwise only replace first occurrence *) ->
    re (** matched groups *) ->
    by:string (** replacement string *) ->
    string (** string to replace in *) ->
    string
end

module Make
    (Cset : Cset.T)
    (Color_map :
      Color_map.T
        with type cp = Cset.cp
         and type letter = Cset.letter
         and type cset_t = Cset.t) =
struct
  module Compile = Compile.Make (Cset) (Color_map)

  type re = Compile.re

  let replace ?(pos = 0) ?len ?(all = true) re ~f s =
    if pos < 0 then invalid_arg "Re.replace";
    let limit =
      match len with
      | None -> String.length s
      | Some l ->
        if l < 0 || pos + l > String.length s then invalid_arg "Re.replace";
        pos + l
    in
    (* buffer into which we write the result *)
    let buf = Buffer.create (String.length s) in
    (* iterate on matched substrings. *)
    let rec iter pos on_match =
      if pos <= limit then
        match
          Compile.match_str ~groups:true ~partial:false re s ~pos
            ~len:(limit - pos)
        with
        | Match substr ->
          let p1 = Group.start_offset substr 0 |> Group.Offset.get_no_check in
          let p2 = Group.stop_offset substr 0 |> Group.Offset.get_no_check in
          if pos = p1 && p1 = p2 && on_match then (
            (* if we matched an empty string right after a match,
             we must manually advance by 1 *)
            if p2 < limit then Buffer.add_char buf s.[p2];
            iter (p2 + 1) false)
          else (
            (* add string between previous match and current match *)
            Buffer.add_substring buf s pos (p1 - pos);
            (* what should we replace the matched group with? *)
            let replacing = f substr in
            Buffer.add_string buf replacing;
            if all then
              (* if we matched an empty string, we must manually advance by 1 *)
              iter
                (if p1 = p2 then (
                   (* a non char could be past the end of string. e.g. $ *)
                   if p2 < limit then Buffer.add_char buf s.[p2];
                   p2 + 1)
                 else p2)
                (p1 <> p2)
            else Buffer.add_substring buf s p2 (limit - p2))
        | Running _ -> ()
        | Failed -> Buffer.add_substring buf s pos (limit - pos)
    in
    iter pos false;
    Buffer.contents buf

  let replace_string ?pos ?len ?all re ~by s =
    replace ?pos ?len ?all re s ~f:(fun _ -> by)
end
