module type T = sig
  type letter
  type t [@@immediate]

  val ( ++ ) : t -> t -> t
  val dummy : t
  val inexistant : t
  val letter : t
  val not_letter : t
  val newline : t
  val lastnewline : t
  val search_boundary : t
  val to_int : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val intersect : t -> t -> bool
  val pp : t Fmt.t
  val to_dyn : t -> Dyn.t
  val from_letter : letter -> t
end

module Make (Cset : Cset.T) : T with type letter = Cset.letter = struct
  type letter = Cset.letter
  type t = int

  let equal (x : int) (y : int) = x = y
  let compare (x : int) (y : int) = compare x y
  let to_int x = x
  let pp = Format.pp_print_int
  let intersect x y = x land y <> 0
  let ( ++ ) x y = x lor y
  let dummy = -1
  let inexistant = 1
  let letter = 2
  let not_letter = 4
  let newline = 8
  let lastnewline = 16
  let search_boundary = 32
  let to_dyn = Dyn.int

  let from_letter =
   fun l ->
    let c = Cset.CodePage.from_letter l in
    match Cset.mem c Cset.cword with
    | true -> letter
    | _ -> (
      match Cset.mem c Cset.nl with
      | true -> not_letter ++ newline
      | _ -> not_letter)

  (* function
  (* Should match [cword] definition *)
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_' | '\170' | '\181' | '\186'
  | '\192' .. '\214'
  | '\216' .. '\246'
  | '\248' .. '\255' -> letter
  | '\n' -> not_letter ++ newline
  | _ -> not_letter *)
end
