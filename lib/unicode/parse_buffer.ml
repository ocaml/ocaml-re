exception Parse_error

module type T = sig
  type t
  type letter

  val create : string -> t
  val junk : t -> unit
  val unget : t -> unit
  val eos : t -> bool
  val test : t -> letter -> bool * int
  val test2 : t -> letter -> letter -> bool
  val get : t -> letter
  val accept : t -> letter -> bool
  val accept_s : t -> string -> bool
  val integer : t -> int option
end

module Make (Cset : Cset.T) = struct
  type letter = Cset.letter
  type t = { str : string; mutable pos : int }

  let create str = { str; pos = 0 }

  let unget t =
    if t.pos = 0 then ()
    else
      let w =
        try Cset.Codec.width_rev String.unsafe_get t.str (t.pos - 1)
        with _ -> 1
      in
      t.pos <- t.pos - w

  let junk t =
    let w =
      try Cset.Codec.width_rev String.unsafe_get t.str t.pos with _ -> 1
    in
    t.pos <- t.pos + w

  let eos t = t.pos = String.length t.str

  let test t letter =
    if not (eos t) then
      let b, pos_next =
        Cset.Codec.Unsafe.unsafe_bytes_with_next_pos t.str t.pos
      in
      let b' = Cset.Codec.to_bytes letter in
      (Bytes.equal b b', pos_next)
    else (false, 0)

  let test2 t letter letter' =
    t.pos + 1 < String.length t.str
    &&
    let r, pos_next = test t letter in
    r
    (* let b = Cset.CodePage.from_letter letter in
    let cp'', pos_next =
      Cset.Codec.Unsafe.unsafe_bytes_with_next_pos t.str t.pos
      |> fun (bytes, pos_next) ->
      (Cset.Codec.from_bytes bytes |> Cset.CodePage.from_letter, pos_next)
    in
    Cset.CodePage.equal cp'' cp *)
    &&
    let b = Cset.Codec.to_bytes letter' in
    let b' = Cset.Codec.Unsafe.unsafe_bytes t.str pos_next in
    Bytes.equal b b'
  (* let cp' = Cset.CodePage.from_letter letter' in
    let cp'' =
      Cset.Codec.Unsafe.unsafe_bytes t.str pos_next
      |> Cset.Codec.from_bytes |> Cset.CodePage.from_letter
    in
    Cset.CodePage.equal cp'' cp' *)

  let accept t c =
    let r, pos_next = test t c in
    if r then t.pos <- pos_next;
    r

  let get t =
    let letter, pos_next =
      Cset.Codec.Unsafe.unsafe_bytes_with_next_pos t.str t.pos
      |> fun (bytes, pos_next) -> (Cset.Codec.from_bytes bytes, pos_next)
    in
    t.pos <- pos_next;
    letter

  let accept_s t s' =
    let len = String.length s' in
    try
      let rec iter max ofs =
        if ofs >= max then (
          t.pos <- t.pos + len;
          true)
        else
          let w = try Cset.Codec.width String.unsafe_get s' ofs with _ -> 1 in
          let b = Cset.Codec.Unsafe.unsafe_slice s' ofs w in
          let b' = Cset.Codec.Unsafe.unsafe_slice t.str (t.pos + ofs) w in
          if Bytes.equal b b' then iter max (ofs + w) else false
        (* let letter =
            Cset.Codec.Unsafe.unsafe_slice s' ofs w |> Cset.Codec.from_bytes
          in
          let letter' =
            Cset.Codec.Unsafe.unsafe_slice t.str (t.pos + ofs) w
            |> Cset.Codec.from_bytes
          in
          if Cset.Codec.equal letter letter' then iter max (ofs + w) else false *)
      in
      iter len 0
    with _ -> false

  let zero = Cset.CodePage.(to_int @@ from_letter @@ of_char '0')

  let rec integer' t i =
    if eos t then Some i
    else
      let cp = get t |> Cset.CodePage.from_letter in
      match Cset.mem cp Cset.cdigit with
      | true ->
        let i' = (10 * i) + (Cset.CodePage.to_int cp - zero) in
        if i' < i then raise Parse_error;
        integer' t i'
      | _ ->
        unget t;
        Some i

  let integer t =
    if eos t then None
    else
      let cp = get t |> Cset.CodePage.from_letter in
      match Cset.mem cp Cset.cdigit with
      | true -> integer' t (Cset.CodePage.to_int cp - zero)
      | _ ->
        unget t;
        None
end
