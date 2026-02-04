(* In reality, this can really be represented as a bool array.

   The representation is best thought of as array of the letters present in the regular expression
   (which is a Cset.t):

   (a, 0), (b, 1), (c, 0), (d, 0), ...

   characters belonging to the same color are represented by sequnces of
   characters with the flag set to 0.
*)

module type T = sig
  type cp
  type letter
  type cset_t
  type color = int
  type t

  module Repr : sig
    type t

    val repr : t -> color -> letter
    val length : t -> int
    val pp : Format.formatter -> t -> unit
  end

  module Table : sig
    type t

    val get : t -> letter -> cp
    val get_letter : t -> cp -> letter
    val translate_colors : t -> cset_t -> cset_t
    val pp : Format.formatter -> t -> unit
  end

  val make : unit -> t
  val flatten : t -> Table.t * Repr.t
  val split : t -> cset_t -> unit
  val pp : Format.formatter -> t -> unit
end

module Make (Cset : Cset.T) = struct
  type letter = Cset.letter
  type cp = Cset.cp
  type cset_t = Cset.t
  type color = int
  type t = int array ref

  let make () : t = ref [||]
  let mem cp t = Array.mem cp !t

  (* t shall be ordered *)
  let add cp (t : t) =
    if mem cp t then ()
    else
      let len = Array.length !t in
      let r = Array.make (len + 1) (-1) in
      let rec iter i max =
        if i >= max then (
          Array.blit !t 0 r 0 i;
          r.(i) <- cp;
          t := r)
        else if Int.compare cp !t.(i) < 0 then (
          Array.blit !t 0 r 0 i;
          r.(i) <- cp;
          Array.blit !t i r (i + 1) (len - i);
          t := r)
        else iter (succ i) max
      in
      iter 0 len

  let pp ppf t =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
      (fun fmt cp -> Format.fprintf fmt "%d" cp)
      ppf (Array.to_list !t)

  let binary_search comp i v =
    let rec loop start finish =
      if finish < start || start > finish then None
      else
        let m = start + ((finish - start) / 2) in
        match comp i (fst v.(m)) with
        | 0 -> Some m
        | 1 -> loop (m + 1) finish
        | _ -> loop start (m - 1)
    in
    loop 0 (Array.length v - 1)

  let cmp_interval i (min, max) =
    if Int.compare i min >= 0 && Int.compare i max <= 0 then 0
    else if Int.compare i min <= 0 then -1
    else 1

  module Repr = struct
    type t = int array

    let pp ppf (t : t) =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        (fun fmt x -> Format.fprintf fmt "%d" x)
        ppf (Array.to_list t)

    let repr (t : t) (color : color) =
      if Array.length t = 0 then Cset.CodePage.(of_int 0 |> to_letter)
      else Cset.CodePage.(t.(color) |> of_int |> to_letter)

    let length = Array.length
  end

  module Table = struct
    type t = ((int * int) * color) array

    let print_one ppf ((c1, c2), color) =
      if Int.equal c1 c2 then Format.fprintf ppf "%d (%d)" c1 color
      else Format.fprintf ppf "%d-%d (%d)" c1 c2 color

    let pp ppf (t : t) =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
        print_one ppf (Array.to_list t)

    (* get the color value as a letter from a cp *)
    let get_letter (t : t) c =
      match binary_search cmp_interval (Cset.CodePage.to_int c) t with
      | None -> Cset.CodePage.(of_int 0 |> to_letter)
      | Some x -> Cset.CodePage.(snd t.(x) |> of_int |> to_letter)

    (* get the color value as a cp from a letter *)
    let get (t : t) letter =
      match
        binary_search cmp_interval
          Cset.CodePage.(from_letter letter |> to_int)
          t
      with
      | None -> Cset.CodePage.of_int 0
      | Some x -> Cset.CodePage.(snd t.(x) |> of_int)

    let translate_colors (cm : t) cset =
      Cset.fold_right cset ~init:Cset.empty ~f:(fun i j l ->
        let start = get_letter cm i in
        let stop = get_letter cm j in
        Cset.union (Cset.cseq start stop) l)
  end

  let len_cany = Cset.fold_left ~f:(fun i _ _ -> succ i) ~init:0 Cset.cany

  let flatten : t -> Table.t * Repr.t =
   fun cm ->
    if Array.length !cm = 0 then
      ( Cset.fold_left Cset.cany ~init:[||] ~f:(fun acc i j ->
          Array.append acc
            [| ((Cset.CodePage.to_int i, Cset.CodePage.to_int j), 0) |]),
        [| Cset.CodePage.(max_t |> to_int) |] )
    else
      let dim = len_cany + Array.length !cm in
      let repr = Array.make dim 0 in
      let table = Array.make dim ((0, 0), 0) in
      table.(0) <- ((0, max 0 @@ pred !cm.(0)), 0);
      repr.(0) <- max 0 @@ pred !cm.(0);
      let len = Array.length !cm in
      let rec iter (a : int array) (color : color) i max =
        if i >= max then
          let cp = pred a.(pred i) in
          let idx = ref i in
          let max_cp = Cset.CodePage.(to_int max_t) in
          let repr_len =
            if Int.compare max_cp cp > 0 then (
              let cset =
                Cset.diff Cset.cany
                  (Cset.cseq
                     Cset.CodePage.(0 |> of_int |> to_letter)
                     Cset.CodePage.(cp |> of_int |> to_letter))
              in
              Cset.iter cset ~f:(fun cp1 cp2 ->
                table.(!idx) <- (Cset.CodePage.(to_int cp1, to_int cp2), color);
                repr.(i) <- Cset.CodePage.(to_int cp2);
                incr idx);
              i + 1)
            else i
          in
          (Array.sub table 0 !idx, Array.sub repr 0 repr_len)
        else (
          table.(i) <- ((a.(pred i), pred a.(i)), color);
          repr.(i) <- pred a.(i);
          iter a (succ color) (succ i) max)
      in
      iter !cm 1 1 len

  (* mark all the endpoints of the intervals of the alphabet. *)
  let split : t -> Cset.t -> unit =
   fun t cset ->
    Cset.iter cset ~f:(fun i j ->
      if not @@ Int.equal (Cset.CodePage.to_int i) 0 then
        add (Cset.CodePage.to_int i) t;
      add (Cset.CodePage.to_int j |> succ) t)
end

module Utf8 = Make (Cset.Utf8)
module Utf16be = Make (Cset.Utf16be)
module Utf16le = Make (Cset.Utf16le)

module Latin1 = struct
  type cp = Cset.Latin1.cp
  type color = int
  type letter = Cset.Latin1.letter
  type cset_t = Cset.Latin1.t
  type t = Bytes.t

  module Repr = struct
    type t = string

    let repr t color =
      t.[color]

    let length = String.length
    let pp ppf t = Format.fprintf ppf "%s" t
  end

  module Table = struct
    type t = string

    let get_letter t c = t.[Cset.Latin1.CodePage.to_int c]

    let get t c =
      Cset.Latin1.CodePage.from_letter (String.unsafe_get t (Char.code c))

    let translate_colors (cm : t) (cset : Cset.Latin1.t) =
      Cset.Latin1.fold_right cset ~init:Cset.Latin1.empty ~f:(fun i j l ->
        let start = get_letter cm i in
        let stop = get_letter cm j in
        Cset.Latin1.union (Cset.Latin1.cseq start stop) l)

    let pp ppf t = Format.fprintf ppf "%s" t
  end

  let make () = Bytes.make 257 '\000'
  let pp ppf t = Format.fprintf ppf "%s" (Bytes.unsafe_to_string t)

  let flatten cm =

    let c = Bytes.create 256 in
    let color_repr = Bytes.create 256 in
    let v = ref 0 in
    Bytes.set c 0 '\000';
    Bytes.set color_repr 0 '\000';
    for i = 1 to 255 do
      if Bytes.get cm i <> '\000' then incr v;
      Bytes.set c i (Char.chr !v);
      Bytes.set color_repr !v (Char.chr i)
    done;
    (Bytes.unsafe_to_string c, Bytes.sub_string color_repr 0 (!v + 1))

  (* mark all the endpoints of the intervals of the char set with the 1 byte *)
  let split t set =
    Cset.Latin1.iter set ~f:(fun i j ->
      Bytes.set t (Cset.Latin1.CodePage.to_int i) '\001';
      Bytes.set t (Cset.Latin1.CodePage.to_int j + 1) '\001')
end
