(* Each of the Cset.t occurring in the AST (or implied by zero-width assertions). *)
type t = Cset.t list ref

module Repr = struct
  type t = string

  let repr t color = t.[Cset.to_int color]
  let length = String.length
end

module Boundary_table = struct
  (** A boundary table is an "array" that maps each byte to the distance to the next
      character that could require a new color. A new color may or may not be necessary
      in the end, because something like [create [ Cset.single 'a' ]] would compute
      boundaries at chars 0 and 'a' and 'b', but 0 and 'b' will end up in the same
      color. *)
  type t = string

  let create csets : t =
    let b =
      Bytes.make 257 '\255'
      (* 257 instead of 256 so we can access past the end of the array in
         unsafe_next_boundary *)
    in
    Bytes.set b 0 '\000';
    List.iter
      (fun cset ->
        Cset.iter cset ~f:(fun c1 c2 ->
          Bytes.set b (Cset.to_int c1) '\000';
          Bytes.set b (Cset.to_int c2 + 1) '\000'))
      csets;
    let skip = ref 0 in
    for i = 255 downto 0 do
      match Bytes.get b i with
      | '\000' -> skip := 0
      | _ ->
        skip := !skip + 1;
        Bytes.set b i (Char.unsafe_chr !skip)
    done;
    Bytes.unsafe_to_string b
  ;;

  let unsafe_next_boundary t i =
    (* i should point at a boundary *)
    i + 1 + Char.code (String.unsafe_get t (i + 1))
  ;;
end

module Table = struct
  type t = string

  let get_char t c = t.[Cset.to_int c]
  let get t c = Cset.of_char (String.unsafe_get t (Char.code c))

  let translate_colors (t : t) boundary_table cset =
    let cs = ref [] in
    let last_version = ref (-1) in
    Cset.iter cset ~f:(fun c1 c2 ->
      (* We use the property that, when iterating over a charset left to right,
         new versions are encountered in increasing order. This holds because
         - new versions are introduced left to right in flatten
         - given a color that's split across multiple ranges, we will
           encounter every such range or no such range (because a given color must
           either be included in [cset], or not intersect with it, otherwise our
           equivalence classes do not respect the structure of [cset]).

         We also know that range starts are necessarily boundaries, so we don't ever
         need to check if we are at a boundary, we just know it ahead of time. *)
      let ci = ref (Cset.to_int c1) in
      while
        let v = Char.code (String.unsafe_get t !ci) in
        if v > !last_version
        then (
          cs := Cset.of_int v :: !cs;
          last_version := v);
        ci := Boundary_table.unsafe_next_boundary boundary_table !ci;
        !ci <= Cset.to_int c2
      do
        ()
      done);
    Cset.union_singles_in_strictly_decreasing_order !cs
  ;;

  module Int_map = Map.Make (Int)

  let equivalence_class_by_color t =
    List.init 256 (fun i -> Cset.to_int (get t (Char.chr i)), i)
    |> List.fold_left
         (fun acc (v, c) ->
           let old_data =
             match Int_map.find_opt v acc with
             | None -> []
             | Some l -> l
           in
           Int_map.add v (Cset.single (Cset.of_int c) :: old_data) acc)
         Int_map.empty
    |> Int_map.map Cset.union_all
    |> Int_map.to_seq
    |> List.of_seq
  ;;

  let to_dyn t =
    let open Dyn in
    equivalence_class_by_color t
    |> List.map (fun (n, cset) -> pair (int n) (Cset.to_dyn cset))
    |> list
  ;;
end

let make () = ref []

let size_cset cset =
  let size = ref 0 in
  Cset.iter cset ~f:(fun c1 c2 -> size := !size + Cset.to_int c2 - Cset.to_int c1 + 1);
  !size
;;

(* [ab] or [^ab] have the same effect on colors: we need to distinguish [ab] from
   [^ab]. But in terms of computation, the one with fewer characters is generally cheaper
   in flatten (for instance cany would need to be evaluated on every boundary, whereas
   its complement Cset.empty doesn't). So cset_or_compl chooses the cheaper one. *)
let cset_or_compl cset = if size_cset cset > 128 then Cset.diff Cset.cany cset else cset
let split (t : t) set = t := cset_or_compl set :: !t

type 'a mutlist =
  | Nil
  | Cons of
      { mutable hd : 'a
      ; tl : 'a mutlist
      }

module Int_list_map = Map.Make (struct
    type t = int mutlist

    let compare = compare
    (* This comparison could be O(length(argument of flatten)) in principle,
       but both map size and number of lookups are bounded by 256 *)
  end)

let flatten t =
  (* t is effectively a map (csetid->char list). We transpose it into a map (char->csetid
     list) stored in var a. Then each unique csetid list becomes a color, giving us
     a map (char->color), in var c.

     In practice, the regex compilation is much faster if we exploit the fact
     that many characters behave the same, so that's what the boundary table is for.
  *)
  let b = Boundary_table.create !t in
  let a = Array.make 256 Nil in
  (let nbits =
     (* +1 to match the +1 to the cset id below *)
     Float.to_int (Float.ceil (Float.log2 (Float.of_int (List.length !t + 1))))
   in
   List.iteri
     (fun csetid cset ->
       let csetid =
         (* +1 so cset id is > 0, which is necessary for [(hd lsl nbits) lsr nbits = hd]
            to correctly compute whether the top nbits are used *)
         csetid + 1
       in
       Cset.iter cset ~f:(fun c1 c2 ->
         let ci = ref (Cset.to_int c1) in
         while
           (match a.(!ci) with
            | Cons ({ hd; tl = _ } as cons) when (hd lsl nbits) lsr nbits = hd ->
              cons.hd <- (hd lsl nbits) lor csetid
            | l -> a.(!ci) <- Cons { hd = csetid; tl = l });
           ci := Boundary_table.unsafe_next_boundary b !ci;
           !ci <= Cset.to_int c2
         do
           ()
         done))
     !t);
  let num_colors = ref 0 in
  let color_by_csetids = ref Int_list_map.empty in
  let c = Bytes.create 256 in
  let color_repr = Bytes.create 256 in
  let last_version = ref 0 in
  Array.iteri
    (fun i csetids ->
      match String.unsafe_get b i with
      | '\000' ->
        let v =
          match Int_list_map.find_opt csetids !color_by_csetids with
          | Some v -> v
          | None ->
            let v = !num_colors in
            color_by_csetids := Int_list_map.add csetids v !color_by_csetids;
            num_colors := !num_colors + 1;
            v
        in
        Bytes.set c i (Char.chr v);
        Bytes.set color_repr v (Char.chr i);
        last_version := v
      | _ ->
        let v = !last_version in
        Bytes.set c i (Char.chr v);
        Bytes.set color_repr v (Char.chr i))
    a;
  Bytes.unsafe_to_string c, b, Bytes.sub_string color_repr 0 !num_colors
;;
