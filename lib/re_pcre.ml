type regexp = Re.re

type flag = [ `CASELESS | `MULTILINE | `ANCHORED ]

type split_result =
  | Text  of string
  | Delim of string
  | Group of int * string
  | NoGroup

type substrings = Re.substrings

let re ?(flags = []) pat =
  let opts = List.map (function
    | `CASELESS -> `Caseless
    | `MULTILINE -> `Multiline
    | `ANCHORED -> `Anchored
  ) flags in
  Re_perl.re ~opts pat

let regexp ?flags pat = Re.compile (re ?flags pat)

let extract ~rex s =
  Re.get_all (Re.exec rex s)

let exec ~rex ?pos s =
  Re.exec rex ?pos s

let get_substring s i =
  Re.get s i

let get_substring_ofs s i =
  Re.get_ofs s i

let pmatch ~rex s =
  Re.execp rex s

let substitute ~rex ~subst str =
  let b = Buffer.create 1024 in
  let rec loop pos =
    if pos >= String.length str then
      Buffer.contents b
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.get_ofs ss 0 in
      let pat = Re.get ss 0 in
      Buffer.add_substring b str pos (start - pos);
      Buffer.add_string b (subst pat);
      loop fin
    ) else (
      Buffer.add_substring b str pos (String.length str - pos);
      loop (String.length str)
    )
  in
  loop 0

let split ~rex str =
  let rec loop accu pos =
    if pos >= String.length str then
      List.rev accu
    else if Re.execp ~pos rex str then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.get_ofs ss 0 in
      let s = String.sub str pos (start - pos) in
      loop (s :: accu) fin
    ) else (
      let s = String.sub str pos (String.length str - pos) in
      loop (s :: accu) (String.length str)
    ) in
  loop [] 0

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
    | '\\' | '^' | '$' | '.' | '[' | '|'
    | '('  | ')' | '?' | '*' | '+' | '{' as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c; incr pos
    | c -> Bytes.unsafe_set buf !pos c; incr pos
  done;
  string_unsafe_sub buf 0 !pos

let full_split ?(max=0) ~rex s =
  if String.length s = 0 then []
  else if max = 1 then [Text s]
  else
    let results = Re.split_full rex s in
    let matches =
      List.map (function
        | `Text s -> [Text s]
        | `Delim d ->
          let matches = Re.get_all_ofs d in
          let delim = Re.get d 0 in
          (Delim delim)::(
            let l = ref [] in
            for i = 1 to Array.length matches - 1 do
              l :=
                (if matches.(i) = (-1, -1)
                 then NoGroup
                 else Group (i, Re.get d i))
                ::(!l)
            done;
            List.rev !l)) results in
    List.concat matches
