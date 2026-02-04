include Re_private_unicode.Import
module Fmt = Re_private_unicode.Fmt
module Dyn = Re_private_unicode.Dyn

module Cset = Re_private_unicode.Cset.Utf8
module Color_map = Re_private_unicode.Color_map.Utf8
(* module Cset = Re_private_unicode.Cset.Latin1
module Color_map = Re_private_unicode.Color_map.Latin1 *)

module Re = struct
  module Category = Re_private_unicode.Category.Make (Cset)
  module Automata = Re_private_unicode.Automata.Make (Cset)
  module Ast = Re_private_unicode.Ast.Make (Cset) (Color_map)
  module Compile = Re_private_unicode.Compile.Make (Cset) (Color_map)
  module Core = Re_private_unicode.Core.Make (Cset) (Color_map)
  module Replace = Re_private_unicode.Replace.Make (Cset) (Color_map)
  include Core
  include Replace
  module View = Re_private_unicode.View.Make (Cset) (Color_map)
  module Emacs = Re_private_unicode.Emacs.Make (Cset) (Color_map)
  module Glob = Re_private_unicode.Glob.Make (Cset) (Color_map)
  module Perl = Re_private_unicode.Perl.Make (Cset) (Color_map)
  module Pcre = Re_private_unicode.Pcre.Make (Cset) (Color_map)
  module Posix = Re_private_unicode.Posix.Make (Cset) (Color_map)
  module Str = Re_private_unicode.Str.Make (Cset) (Color_map)
end

let printf = Printf.printf

let t re s =
  let re = Re.compile re in
  let group = Re.exec_opt re s in
  Format.printf "%a@." (Fmt.opt Re.Group.pp) group

let re_whitespace = Re.Pcre.regexp "[\t ]+"
let re_eol = Re.compile Re.eol
let re_bow = Re.compile Re.bow
let re_eow = Re.compile Re.eow

let strings =
  Format.printf "[%a]@." Fmt.(list ~pp_sep:(Fmt.lit "; ") Fmt.quoted_string)

let re_empty = Re.Posix.compile_pat ""

let invalid_argument f =
  try ignore (f ())
  with Invalid_argument s -> Format.printf "Invalid_argument %S@." s

let ignore_or_exception f =
  try ignore (f ())
  with
  | Re_private_unicode.Uucodecs.CodecError -> printf "\"CodecError\"\n"
  | exn -> Format.printf "%S@." (Printexc.to_string exn)

let exec_partial_detailed ?pos re s =
  let re = Re.compile re in
  let res = Re.exec_partial_detailed ?pos re s in
  match res with
  | `Mismatch -> Format.printf "`Mismatch@."
  | `Partial position -> Format.printf "`Partial %d@." position
  | `Full groups ->
    Re.Group.all_offset groups |> Array.to_list
    |> List.map ~f:(fun (a, b) ->
         Printf.sprintf "%d,%d,%s" a b
           (match String.sub s a (b - a) with
           | exception Invalid_argument _ -> "<No match>"
           | s -> Printf.sprintf "%S" s))
    |> String.concat ";"
    |> Format.printf "`Full [|%s|]@."

let or_not_found f fmt v =
  try f fmt (v ()) with
  | Not_found -> Format.fprintf fmt "Not_found"
  | exn -> Format.fprintf fmt "%s" (Printexc.to_string exn)

let array f fmt v =
  Format.fprintf fmt "[| %a |]"
    (Fmt.list ~pp_sep:(Fmt.lit "; ") f)
    (Array.to_list v)

let offset fmt (x, y) = Format.fprintf fmt "(%d, %d)" x y

let test_re ?pos ?len r s =
  let offsets () = Re.Group.all_offset (Re.exec ?pos ?len (Re.compile r) s) in
  Format.printf "%a@." (or_not_found (array offset)) offsets

let rec sexp_of_dyn (t : Dyn.t) : Base.Sexp.t =
  match t with
  | Int i -> Atom (Int.to_string i)
  | String s -> Atom s
  | Tuple xs -> List (List.map xs ~f:sexp_of_dyn)
  | Enum s -> Atom s
  | Array xs -> List (List.map ~f:sexp_of_dyn @@ Array.to_list xs)
  | List xs -> List (List.map ~f:sexp_of_dyn xs)
  | Variant (name, []) -> Atom name
  | Variant (name, xs) -> (
    let xs = List.map xs ~f:sexp_of_dyn in
    match xs with [] -> List [] | xs -> List (Atom name :: xs))
  | Record fields ->
    List
      (List.filter_map fields ~f:(fun (name, v) ->
         match sexp_of_dyn v with
         | List [] -> None
         | sexp -> Some (Base.Sexp.List [ Atom name; sexp ])))

let print_dyn dyn = sexp_of_dyn dyn |> Base.Sexp.to_string_hum |> print_endline

let test f string =
  match f string with
  | Ok res -> print_dyn (Re.Ast.to_dyn res)
  | Error _ -> assert false

let string_make_of_int i =
  let bytes = Bytes.create 4 in
  let w = Cset.Codec.set bytes 0 (Cset.CodePage.of_char @@ Char.chr i) in
  Bytes.sub_string bytes 0 w

let string_make_of_char c =
  let buf = Buffer.create 4 in
  Cset.Codec.add buf (Cset.CodePage.of_char c);
  Buffer.contents buf
