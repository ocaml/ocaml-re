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

module type Categories = sig
  type c

  val cany : (c * c) list
  val ascii : (c * c) list
  val cdigit : (c * c) list
  val cupper : (c * c) list
  val clower : (c * c) list
  val cword : (c * c) list
  val calpha : (c * c) list
  val calnum : (c * c) list
  val xdigit : (c * c) list
  val lower : (c * c) list
  val upper : (c * c) list
  val alpha : (c * c) list
  val alnum : (c * c) list
  val wordc : (c * c) list
  val nl : (c * c) list
  val blank : (c * c) list
  val space : (c * c) list
  val cntrl : (c * c) list
  val graph : (c * c) list
  val print : (c * c) list
  val punct : (c * c) list
end

module type CodePage = sig
  type t [@@immediate]
  type letter

  val to_letter : t -> letter
  val from_letter : letter -> t
  val null : t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val succ : t -> t
  val pred : t -> t
  val max : t -> t -> t
  val min : t -> t -> t
  val max_t : t
  val min_t : t
  val offset : int -> t -> t
  val to_int : t -> int
  val of_int : int -> t
  val of_char : char -> letter
  val to_char : letter -> char
  val pp : Format.formatter -> t -> unit

  module Categories : Categories with type c := t
end

module type T = sig
  type cp [@@immediate]
  type letter
  type t

  module CodePage : CodePage with type t := cp and type letter := letter
  module Codec : Uucodecs.T with type letter = letter

  val equal : t -> t -> bool
  val iter : t -> f:(cp -> cp -> unit) -> unit
  val union : t -> t -> t
  val union_all : t list -> t
  val intersect_all : t list -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val empty : t
  val single : cp -> t
  val add : cp -> t -> t
  val mem : cp -> t -> bool
  val case_insens : t -> t
  val cany : t
  val ascii : t
  val cdigit : t
  val cupper : t
  val clower : t
  val calpha : t
  val cword : t
  val notnl : t
  val nl : t
  val cseq : letter -> letter -> t
  val set : string -> t
  val blank : t
  val space : t
  val xdigit : t
  val lower : t
  val upper : t
  val alpha : t
  val alnum : t
  val wordc : t
  val cntrl : t
  val graph : t
  val print : t
  val punct : t
  val pp : Format.formatter -> t -> unit
  val one_c : t -> cp option
  val fold_left : t -> init:'acc -> f:('acc -> cp -> cp -> 'acc) -> 'acc
  val fold_right : t -> init:'acc -> f:(cp -> cp -> 'acc -> 'acc) -> 'acc
  val hash : t -> int
  val compare : t -> t -> int

  module CSetMap : Map.S with type key = int * t

  val csingle : letter -> t
  val is_empty : t -> bool
  val prepend : t -> 'a list -> (t * 'a list) list -> (t * 'a list) list
  val pick : t -> cp
  val offset : int -> t -> t
  val to_dyn : t -> Dyn.t
end

module UcharCp : CodePage with type letter = Uchar.t = struct
  type t = int
  type letter = Uchar.t

  external to_int : t -> int = "%identity"
  external of_int : int -> t = "%identity"

  let of_char = Uchar.of_char
  let to_char = Uchar.to_char
  let min = Int.min
  let max = Int.max
  let max_t = 0x10ffff
  let min_t = 0
  let to_letter c = Uchar.unsafe_of_int @@ of_int c
  let from_letter u = of_int @@ Uchar.to_int u
  let equal = fun cp cp' -> Int.equal (of_int cp) (of_int cp')
  let compare = fun cp cp' -> Int.compare (of_int cp) (of_int cp')

  let succ t =
    try Uchar.of_int t |> Uchar.succ |> Uchar.to_int with _ -> max_t

  let pred t =
    try Uchar.of_int t |> Uchar.pred |> Uchar.to_int with _ -> min_t

  let offset =
   fun ofs cp ->
    let cp' = cp + ofs in
    if cp' > max_t then max_t
    else if cp <= 0xd7ff && cp' > 0xd7ff then 0xe000 - 0xd7ff + cp'
    else cp'

  let null = of_int (-1)
  let pp ppf t = Format.fprintf ppf "%d" t

  module Categories = struct
    include Unicode.Regexp

    let ascii = [ (0x00, 0x7F) ]
    let cany = [ (0x0000, 0xd7ff); (0xe000, 0x10ffff) ]
    let blank = [ (0x0009, 0x0009); (0x0020, 0x0020) ]
  end
end

module Make
    (Codec : Uucodecs.T)
    (Cp : CodePage with type letter = Codec.letter) :
  T with type cp = Cp.t and type letter = Codec.letter = struct
  type cp = Cp.t
  type letter = Codec.letter

  module CodePage = Cp
  module Codec = Codec
  include CodePage.Categories
  open! Import

  type t = (cp * cp) list

  (* type t = (cp * cp) array *)

  let equal_pair (x, y) (x', y') = CodePage.equal x x' && CodePage.equal y y'

  let compare_pair (x, y) (x', y') =
    match CodePage.compare x x' with 0 -> CodePage.compare y y' | x -> x

  let equal = List.equal ~eq:equal_pair
  let compare : t -> t -> int = List.compare ~cmp:compare_pair

  let print_one ppf (c1, c2) =
    if CodePage.equal c1 c2 then Format.fprintf ppf "%a" CodePage.pp c1
    else Format.fprintf ppf "%a-%a" CodePage.pp c1 CodePage.pp c2

  let pp ppf t =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
      print_one ppf t

  let rec union l l' =
    match (l, l') with
    | _, [] -> l
    | [], _ -> l'
    | (c1, c2) :: r, (c1', c2') :: r' ->
      if CodePage.compare c1' (CodePage.succ c2) > 0 then (c1, c2) :: union r l'
      else if CodePage.compare c1 (CodePage.succ c2) > 0 then
        (c1', c2') :: union l r'
      else if CodePage.compare c2' c2 > 0 then
        union r ((CodePage.min c1 c1', c2') :: r')
      else union ((CodePage.min c1 c1', c2) :: r) r'

  let rec inter l l' =
    match (l, l') with
    | _, [] -> []
    | [], _ -> []
    | (c1, c2) :: r, (c1', c2') :: r' ->
      if CodePage.compare c1' c2 > 0 then inter r l'
      else if CodePage.compare c1 c2' > 0 then inter l r'
      else if CodePage.compare c2' c2 > 0 then
        (CodePage.max c1 c1', c2) :: inter r l'
      else (CodePage.max c1 c1', c2') :: inter l r'

  let rec diff l l' =
    match (l, l') with
    | _, [] -> l
    | [], _ -> []
    | (c1, c2) :: r, (c1', c2') :: r' ->
      if CodePage.compare c1' c2 > 0 then (c1, c2) :: diff r l'
      else if CodePage.compare c1 c2' > 0 then diff l r'
      else
        let r'' =
          if CodePage.compare c2 c2' > 0 then (CodePage.succ c2', c2) :: r
          else r
        in
        if CodePage.compare c1' c1 > 0 then
          (c1, CodePage.pred c1') :: diff r'' r'
        else diff r'' r'

  let single c = [ (c, c) ]
  let csingle letter = single @@ CodePage.from_letter letter
  let add c s = union (single c) s
  let seq c c' = if CodePage.compare c c' <= 0 then [ (c, c') ] else [ (c', c) ]

  let offset ofs =
    List.fold_left ~init:[] ~f:(fun acc (c1, c2) ->
      union acc (seq (CodePage.offset ofs c1) (CodePage.offset ofs c2)))

  let empty = []
  let union_all : t list -> t = List.fold_left ~init:empty ~f:union
  let intersect_all : t list -> t = List.fold_left ~init:cany ~f:inter

  let rec mem (c : cp) cset =
    match cset with
    | [] -> false
    | (c1, c2) :: rem ->
      if CodePage.compare c2 c >= 0 then CodePage.compare c c1 >= 0
      else mem c rem

  (****)

  let rec hash_rec = function
    | [] -> 0
    | (i, j) :: r ->
      CodePage.to_int i + (13 * CodePage.to_int j) + (257 * hash_rec r)

  let hash l = hash_rec l land 0x3fffffff

  (****)

  let to_dyn t =
    let open Dyn in
    match t with
    | [ (x, y) ] when CodePage.equal x y -> int @@ CodePage.to_int x
    | _ ->
      List.map t ~f:(fun (x, y) ->
        pair (int @@ CodePage.to_int x) (int @@ CodePage.to_int y))
      |> list

  let rec iter t ~f =
    match t with
    | [] -> ()
    | (x, y) :: xs ->
      f x y;
      iter xs ~f

  let one_c = function
    | [ (i, j) ] when CodePage.equal i j -> Some i
    | _ -> None

  module CSetMap = Map.Make (struct
    type t = int * (cp * cp) list

    let compare (i, u) (j, v) =
      let c = Int.compare i j in
      if c <> 0 then c else compare u v
  end)

  let fold_left t ~init ~f =
    List.fold_left ~f:(fun acc (x, y) -> f acc x y) t ~init

  let fold_right t ~init ~f =
    List.fold_right ~f:(fun (x, y) acc -> f x y acc) t ~init

  let is_empty = function [] -> true | _ -> false

  let rec prepend s x l =
    match (s, l) with
    | [], _ -> l
    | _r, [] -> []
    | (_c, c') :: r, ([ (d, _d') ], _x') :: _r' when CodePage.compare d c' > 0
      ->
      prepend r x l
    | (c, c') :: r, ([ (d, d') ], x') :: r' ->
      if CodePage.compare d c >= 0 then
        if CodePage.compare d c' > 0 then
          ([ (d, c') ], x @ x')
          :: prepend r x (([ (CodePage.succ c', d') ], x') :: r')
        else ([ (d, d') ], x @ x') :: prepend s x r'
      else if CodePage.compare d' c > 0 then ([ (d, d') ], x') :: prepend s x r'
      else
        ([ (d, CodePage.pred c) ], x') :: prepend s x (([ (c, d') ], x') :: r')
    | _ -> assert false

  let pick = function [] -> invalid_arg "Re_cset.pick" | (x, _) :: _ -> x

  let cseq u u' =
    inter cany @@ seq (CodePage.from_letter u) (CodePage.from_letter u')

  (* let rg = cseq *)
  (* let uchar = csingle *)
  (* let cadd c s = add (of_uchar c) s *)

  (* simple case mapping implemented.
     see https://www.unicode.org/versions/Unicode17.0.0/core-spec/chapter-3/#G33992
     TODO: full case folding ? *)
  let case_insens s =
    let l = ref [] in
    iter s ~f:(fun cp1 cp2 ->
      let i = CodePage.to_int cp1 in
      let j = CodePage.to_int cp2 in
      for n = i to j do
        List.iter ~f:(fun cp -> l := single (CodePage.of_int cp) :: !l)
        @@ Unicode.get_simple_case_folding n
      done);
    union_all (s :: !l)

  let notnl = diff cany nl

  let set str =
    Codec.fold_left (fun acc letter -> union acc (csingle letter)) empty str
end

module Utf8 : T with type letter = Uchar.t = Make (Uucodecs.Utf8) (UcharCp)

module Utf16be : T with type letter = Uchar.t =
  Make (Uucodecs.Utf16be) (UcharCp)

module Utf16le : T with type letter = Uchar.t =
  Make (Uucodecs.Utf16le) (UcharCp)

module CharCp : CodePage with type letter = char = struct
  type t = int
  type letter = Char.t

  let to_int = Fun.id
  let of_int = Fun.id
  let to_letter i = Char.chr i
  let from_letter c = Char.code c
  let of_char = Fun.id
  let to_char = Fun.id
  let equal = fun cp cp' -> Int.equal (of_int cp) (of_int cp')
  let compare = fun cp cp' -> Int.compare (of_int cp) (of_int cp')
  let max = Int.max
  let min = Int.min
  let max_t = 255
  let min_t = 0
  let succ = succ
  let pred = pred
  let offset = fun ofs cp -> cp + ofs
  let null = -1
  let pp ppf t = Format.fprintf ppf "%d" t

  module Categories = struct
    let cany = [ (0x00, 0xFF) ]
    let ascii = [ (0x00, 0x7F) ]
    let cdigit = [ (0x30, 0x39) ]
    let cupper = [ (0x41, 0x5A) ]
    let upper = [ (0x41, 0x5A); (0xC0, 0xD6); (0xD8, 0xDE) ]
    let clower = [ (0x61, 0x7A) ]
    let space = [ (0x09, 0x0D); (0x32, 0x32) ]
    let xdigit = [ (0x30, 0x39); (0x41, 0x46); (0x61, 0x66) ]

    let calpha =
      [
        (0x41, 0x5A);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xDE);
        (0xDF, 0xDF);
        (0xFF, 0xFF);
      ]

    let calnum =
      [
        (0x30, 0x39);
        (0x41, 0x5A);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xDE);
        (0xDF, 0xDF);
        (0xFF, 0xFF);
      ]

    let cword =
      [
        (0x30, 0x39);
        (0x41, 0x5A);
        (0x5F, 0x5F);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xDE);
        (0xDF, 0xDF);
        (0xFF, 0xFF);
      ]

    let nl = [ (0x0A, 0x0A) ]

    (* CR-someday rgrinberg: this [lower] doesn't match [clower] *)
    let lower = [ (0x61, 0x7A); (0xB5, 0xB5); (0xDF, 0xF6); (0xF8, 0xFF) ]

    let alpha =
      [
        (0x41, 0x5A);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xF6);
        (0xF8, 0xFF);
      ]

    let alnum =
      [
        (0x30, 0x39);
        (0x41, 0x5A);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xF6);
        (0xF8, 0xFF);
      ]

    let wordc =
      [
        (0x30, 0x39);
        (0x41, 0x5A);
        (0x5F, 0x5F);
        (0x61, 0x7A);
        (0xAA, 0xAA);
        (0xB5, 0xB5);
        (0xBA, 0xBA);
        (0xC0, 0xD6);
        (0xD8, 0xF6);
        (0xF8, 0xFF);
      ]

    let cntrl = [ (0x00, 0x1F); (0x7F, 0x9F) ]
    let graph = [ (0x21, 0x7E); (0xA0, 0xFF) ]
    let print = [ (0x20, 0x7E); (0xA0, 0xFF) ]

    let punct =
      [
        (0x21, 0x2F);
        (0x3A, 0x40);
        (0x5B, 0x60);
        (0x7B, 0x7E);
        (0xA0, 0xA9);
        (0xAB, 0xB4);
        (0xB6, 0xB9);
        (0xBB, 0xBF);
        (0xD7, 0xD7);
        (0xF7, 0xF7);
      ]

    let blank = [ (0x0009, 0x0009); (0x0020, 0x0020) ]
  end
end

module Latin1 : T with type letter = Char.t = Make (Uucodecs.Latin1) (CharCp)
