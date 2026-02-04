exception CodecError
exception End_of_data

module type T = sig
  type letter

  val version : string

  module Unsafe : sig
    val unsafe_slice : string -> int -> int -> bytes
    val unsafe_bytes_with_next_pos : string -> int -> bytes * int
    val unsafe_bytes : string -> int -> bytes
    val unsafe_bytes_rev_with_next_pos : string -> int -> bytes * int
    val unsafe_bytes_rev : string -> int -> bytes
  end

  val max_width : int
  val rep : letter
  val new_line : letter
  val null : letter
  val byte_length : letter -> int
  val equal : letter -> letter -> bool
  val compare : letter -> letter -> int
  val from_int : int -> letter
  val to_int : letter -> int
  val from_bytes : bytes -> letter
  val to_bytes : letter -> bytes
  val set : bytes -> int -> letter -> int
  val add : Buffer.t -> letter -> unit
  val width : ('a -> int -> char) -> 'a -> int -> int
  val width_rev : ('a -> int -> char) -> 'a -> int -> int

  (* encoder *)

  type 'a enc

  val encoder_make : (int -> 'a -> bytes -> 'a) -> 'a enc
  val encoder_add : 'a enc -> 'a -> bytes -> 'a
  val encoder_flush : 'a enc -> 'a -> 'a
  val is_encoded : letter -> bool
  val iter : (letter -> unit) -> string -> unit
  val fold_left : ('acc -> letter -> 'acc) -> 'acc -> string -> 'acc
  val fold_right : (letter -> 'acc -> 'acc) -> string -> 'acc -> 'acc
  val to_list : string -> letter list
  val to_seq : string -> letter Seq.t

  (* pretty printer *)
  val pp : Format.formatter -> letter -> unit
  val dump : Format.formatter -> letter -> unit
end

module type Properties = sig
  (** normalization form (see {!/uunf/UUnf/page-index}).*)
  val form : Uunf.form

  (** see
      {{:https://unicode.org/reports/tr15/#Stream_Safe_Text_Format}Stream-Safe
       Text Format}.

      Note that if [stream_safe = true], then there is at least one starter
      every 31 characters. A starter is a character, in a Unicode Normalization
      Form, characterized by its canonical combining class value of zero (see
      {!Uunf.ccc}). The Stream Safe Text Format ensures that one combining
      grapheme joiner (U+034F) is inserted every 30 non-starters.

      It is interesting to note that its
      {b canonical combining class value is zero} and its
      {b properties NFC_QC, NFD_QC, NFKC_QC and NFKD_QC are all `True}.*)
  val stream_safe : bool
end

module DefaultProperties : Properties = struct
  let form = `NFC
  let stream_safe = true
end

module type Skel = sig
  type letter

  include Properties

  val byte_length : letter -> int
  val equal : letter -> letter -> bool
  val compare : letter -> letter -> int
  val from_int : int -> letter
  val to_int : letter -> int
  val from_bytes : bytes -> letter
  val to_bytes : letter -> bytes
  val set : bytes -> int -> letter -> int
  val add : Buffer.t -> letter -> unit
  val width : ('a -> int -> char) -> 'a -> int -> int
  val width_rev : ('a -> int -> char) -> 'a -> int -> int

  (*pretty printer *)
  val pp : Format.formatter -> letter -> unit
  val dump : Format.formatter -> letter -> unit
end

module MakeUtf8Skel (P : Properties) : Skel with type letter = Uchar.t = struct
  type letter = Uchar.t

  include P

  let byte_length t = Uchar.utf_8_byte_length t
  let equal = Uchar.equal
  let compare = Uchar.compare
  let from_int = fun i -> try Uchar.unsafe_of_int i with _ -> raise CodecError
  let to_int = Uchar.to_int

  let int_to_4_uint8 n =
    if n > 0x10FFFF then raise CodecError
    else
      let bytes = Bytes.create 4 in
      List.iteri (Bytes.set_uint8 bytes)
        [
          0xF0 lor (n lsr 18);
          0x80 lor ((n lsr 12) land 0x3F);
          0x80 lor ((n lsr 6) land 0x3F);
          0x80 lor (n land 0x3F);
        ];
      bytes

  let int_to_3_uint8 n =
    if n > 0xFFFF then int_to_4_uint8 n
    else
      let bytes = Bytes.create 3 in
      List.iteri (Bytes.set_uint8 bytes)
        [
          0xE0 lor (n lsr 12);
          0x80 lor ((n lsr 6) land 0x3F);
          0x80 lor (n land 0x3F);
        ];
      bytes

  let int_to_2_uint8 n =
    if n > 0x07FF then int_to_3_uint8 n
    else
      let bytes = Bytes.create 2 in
      List.iteri (Bytes.set_uint8 bytes)
        [ 0xC0 lor (n lsr 6); 0x80 lor (n land 0x3F) ];
      bytes

  let to_bytes u =
    let n = Uchar.to_int u in
    if n > 0x07F then int_to_2_uint8 n
    else
      let bytes = Bytes.create 1 in
      Bytes.set_uint8 bytes 0 n;
      bytes

  let set bytes pos t =
    let t_bytes = to_bytes t in
    let len = Bytes.length t_bytes in
    Bytes.blit t_bytes 0 bytes pos len;
    len

  let add buf t = Buffer.add_bytes buf @@ to_bytes t

  let pp : Format.formatter -> letter -> unit =
   fun ppf cp ->
    let len = byte_length cp in
    let b = Buffer.create len in
    add b cp;
    let gc = Buffer.contents b in
    if gc = "" then ()
    else (
      Format.fprintf ppf "@<1>%s" gc;
      Buffer.clear b)

  let dump : Format.formatter -> letter -> unit =
   fun ppf t -> Format.fprintf ppf "{U+%04X}" (Uchar.to_int t)

  let int_of_2_uint8 n1 n2 =
    if n1 < 0xc2 || 0xdf < n1 then raise CodecError;
    if n2 < 0x80 || 0xbf < n2 then raise CodecError;
    if n2 lsr 6 != 0b10 then raise CodecError;
    ((n1 land 0x1f) lsl 6) lor (n2 land 0x3f)

  let int_of_3_uint8 n1 n2 n3 =
    if n1 = 0xe0 then (
      if n2 < 0xa0 || 0xbf < n2 then raise CodecError;
      if n3 < 0x80 || 0xbf < n3 then raise CodecError)
    else (
      if n1 < 0xe1 || 0xef < n1 then raise CodecError;
      if n2 < 0x80 || 0xbf < n2 then raise CodecError;
      if n3 < 0x80 || 0xbf < n3 then raise CodecError);
    if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 then raise CodecError;
    let p =
      ((n1 land 0x0f) lsl 12) lor ((n2 land 0x3f) lsl 6) lor (n3 land 0x3f)
    in
    if p >= 0xd800 && p <= 0xdf00 then raise CodecError;
    p

  let int_of_4_uint8 n1 n2 n3 n4 =
    if n1 = 0xf0 then (
      if n2 < 0x90 || 0xbf < n2 then raise CodecError;
      if n3 < 0x80 || 0xbf < n3 then raise CodecError;
      if n4 < 0x80 || 0xbf < n4 then raise CodecError)
    else if n1 = 0xf4 then (
      if n2 < 0x80 || 0x8f < n2 then raise CodecError;
      if n3 < 0x80 || 0xbf < n3 then raise CodecError;
      if n4 < 0x80 || 0xbf < n4 then raise CodecError)
    else (
      if n1 < 0xf1 || 0xf3 < n1 then raise CodecError;
      if n2 < 0x80 || 0xbf < n2 then raise CodecError;
      if n3 < 0x80 || 0xbf < n3 then raise CodecError;
      if n4 < 0x80 || 0xbf < n4 then raise CodecError);
    if n2 lsr 6 != 0b10 || n3 lsr 6 != 0b10 || n4 lsr 6 != 0b10 then
      raise CodecError;
    ((n1 land 0x07) lsl 18)
    lor ((n2 land 0x3f) lsl 12)
    lor ((n3 land 0x3f) lsl 6)
    lor (n4 land 0x3f)

  let from_bytes bytes =
    match Bytes.length bytes with
    | 1 -> Bytes.get_uint8 bytes 0 |> Uchar.unsafe_of_int
    | 2 ->
      int_of_2_uint8 (Bytes.get_uint8 bytes 0) (Bytes.get_uint8 bytes 1)
      |> Uchar.unsafe_of_int
    | 3 ->
      int_of_3_uint8 (Bytes.get_uint8 bytes 0) (Bytes.get_uint8 bytes 1)
        (Bytes.get_uint8 bytes 2)
      |> Uchar.unsafe_of_int
    | 4 ->
      int_of_4_uint8 (Bytes.get_uint8 bytes 0) (Bytes.get_uint8 bytes 1)
        (Bytes.get_uint8 bytes 2) (Bytes.get_uint8 bytes 3)
      |> Uchar.unsafe_of_int
    | _ -> raise CodecError

  let width f x pos =
    let n = f x pos |> Char.code in
    if n < 0x80 then 1
    else if n < 0xc2 then raise CodecError
    else if n < 0xe0 then 2
    else if n < 0xf0 then 3
    else if n < 0xf5 then 4
    else raise CodecError

  let width_rev f x pos =
    let f = fun v ofs -> f v ofs |> Char.code in
    let decode_length_pos_3 fn v pos =
      let byte = fn v (pos - 3) in
      if byte > 0xEF && byte < 0xF5 then 4 else raise CodecError
    in
    let decode_length_pos_2 fn v pos =
      let byte = fn v (pos - 2) in
      if byte > 0xDF && byte < 0xF0 then 3 else decode_length_pos_3 fn v pos
    in
    let decode_length_pos_1 fn v pos =
      let byte = fn v (pos - 1) in
      if byte > 0xC1 && byte < 0xE0 then 2 else decode_length_pos_2 fn v pos
    in
    let decode_length_pos_0 fn v pos =
      let byte = fn v pos in
      if byte < 0x80 then 1 else decode_length_pos_1 fn v pos
    in
    decode_length_pos_0 f x pos
end

external unsafe_get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16u"

external unsafe_set_uint16_ne : bytes -> int -> int -> unit
  = "%caml_bytes_set16u"

external swap16 : int -> int = "%bswap16"

module MakeUtf16beSkel (P : Properties) : Skel with type letter = Uchar.t =
struct
  type letter = Uchar.t

  include P

  let byte_length t = Uchar.utf_16_byte_length t
  let equal = Uchar.equal
  let compare = Uchar.compare
  let from_int = Uchar.unsafe_of_int
  let to_int = Uchar.to_int

  let unsafe_get_uint16_be b i =
    if Sys.big_endian then unsafe_get_uint16_ne b i
    else swap16 (unsafe_get_uint16_ne b i)

  let unsafe_set_uint16_be b i x =
    if Sys.big_endian then unsafe_set_uint16_ne b i x
    else unsafe_set_uint16_ne b i (swap16 x)

  let to_bytes u =
    let n = Uchar.to_int u in
    if n > 0xffff then (
      if n > 0x10ffff then raise CodecError
      else
        let n' = n - 0x10000 in
        let hi = 0xd800 lor (n' lsr 10) in
        let lo = 0xdc00 lor (n' land 0x3ff) in
        let bytes = Bytes.create 4 in
        unsafe_set_uint16_be bytes 0 hi;
        unsafe_set_uint16_be bytes 2 lo;
        bytes)
    else
      let bytes = Bytes.create 2 in
      unsafe_set_uint16_be bytes 0 n;
      bytes

  let set bytes pos u =
    let n = Uchar.to_int u in
    if n > 0xffff then (
      if n > 0x10ffff then raise CodecError
      else
        let n' = n - 0x10000 in
        let hi = 0xd800 lor (n' lsr 10) in
        let lo = 0xdc00 lor (n' land 0x3ff) in
        unsafe_set_uint16_be bytes pos hi;
        unsafe_set_uint16_be bytes (pos + 2) lo;
        4)
    else (
      unsafe_set_uint16_be bytes pos n;
      2)

  let add buf t = Buffer.add_bytes buf @@ to_bytes t

  let pp : Format.formatter -> letter -> unit =
   fun ppf cp ->
    let len = byte_length cp in
    let b = Buffer.create len in
    add b cp;
    let gc = Buffer.contents b in
    if gc = "" then ()
    else (
      Format.fprintf ppf "@<1>%s" gc;
      Buffer.clear b)

  let dump : Format.formatter -> letter -> unit =
   fun ppf t -> Format.fprintf ppf "{U+%04X}" (Uchar.to_int t)

  let int_of_2_uint8 bytes =
    let w = unsafe_get_uint16_be bytes 0 in
    if w < 0xd800 || 0xdfff < w then w else raise CodecError

  let int_of_4_uint8 bytes =
    let hi = unsafe_get_uint16_be bytes 0 in
    let lo = unsafe_get_uint16_be bytes 2 in
    if hi < 0xdc00 || hi > 0xdfff then raise CodecError
    else (((hi land 0x3ff) lsl 10) lor (lo land 0x3ff)) + 0x10000

  let from_bytes bytes =
    match Bytes.length bytes with
    | 2 -> int_of_2_uint8 bytes |> Uchar.unsafe_of_int
    | 4 -> int_of_4_uint8 bytes |> Uchar.unsafe_of_int
    | _ -> raise CodecError

  let width : ('a -> int -> char) -> 'a -> int -> int =
   fun f x pos ->
    let buf = Buffer.create 2 in
    Buffer.add_char buf @@ f x pos;
    Buffer.add_char buf @@ f x (pos + 1);
    let hi = unsafe_get_uint16_be (Buffer.to_bytes buf) 0 in
    Buffer.clear buf;
    if hi < 0xd800 || 0xdfff < hi then 2
    else if hi <= 0xdbff then 4
    else raise CodecError

  let width_rev f x pos =
    let decode_length_pos_1 fn v ofs buf =
      Buffer.add_char buf @@ fn v (ofs - 3);
      Buffer.add_char buf @@ fn v (ofs - 2);
      let hi = unsafe_get_uint16_be (Buffer.to_bytes buf) 0 in
      Buffer.clear buf;
      if hi <= 0xdbff then 4 else raise CodecError
    in
    let decode_length_pos_0 fn v ofs buf =
      Buffer.add_char buf @@ fn v (ofs - 1);
      Buffer.add_char buf @@ fn v ofs;
      let hi = unsafe_get_uint16_be (Buffer.to_bytes buf) 0 in
      Buffer.reset buf;
      if hi < 0xd800 || 0xdfff < hi then 2 else decode_length_pos_1 f v ofs buf
    in
    let buf = Buffer.create 2 in
    decode_length_pos_0 f x pos buf
end

module MakeUtf16leSkel (P : Properties) : Skel with type letter = Uchar.t =
struct
  type letter = Uchar.t

  include P

  let byte_length t = Uchar.utf_16_byte_length t
  let equal = Uchar.equal
  let compare = Uchar.compare
  let from_int = Uchar.unsafe_of_int
  let to_int = Uchar.to_int

  let unsafe_get_uint16_le b i =
    if Sys.big_endian then swap16 (unsafe_get_uint16_ne b i)
    else unsafe_get_uint16_ne b i

  let unsafe_set_uint16_le b i x =
    if Sys.big_endian then unsafe_set_uint16_ne b i (swap16 x)
    else unsafe_set_uint16_ne b i x

  let to_bytes u =
    let n = Uchar.to_int u in
    if n > 0xffff then (
      if n > 0x10ffff then raise CodecError
      else
        let n' = n - 0x10000 in
        let hi = 0xd800 lor (n' lsr 10) in
        let lo = 0xdc00 lor (n' land 0x3ff) in
        let bytes = Bytes.create 4 in
        unsafe_set_uint16_le bytes 0 hi;
        unsafe_set_uint16_le bytes 2 lo;
        bytes)
    else
      let bytes = Bytes.create 2 in
      unsafe_set_uint16_le bytes 0 n;
      bytes

  let set bytes pos u =
    let n = Uchar.to_int u in
    if n > 0xffff then (
      if n > 0x10ffff then raise CodecError
      else
        let n' = n - 0x10000 in
        let hi = 0xd800 lor (n' lsr 10) in
        let lo = 0xdc00 lor (n' land 0x3ff) in
        unsafe_set_uint16_le bytes pos hi;
        unsafe_set_uint16_le bytes (pos + 2) lo;
        4)
    else (
      unsafe_set_uint16_le bytes pos n;
      2)

  let add buf t = Buffer.add_bytes buf @@ to_bytes t

  let pp : Format.formatter -> letter -> unit =
   fun ppf t ->
    let len = byte_length t in
    let b = Buffer.create len in
    add b t;
    let gc = Buffer.contents b in
    if gc = "" then ()
    else (
      Format.fprintf ppf "@<1>%s" gc;
      Buffer.clear b)

  let dump : Format.formatter -> letter -> unit =
   fun ppf t -> Format.fprintf ppf "{U+%04X}" (Uchar.to_int t)

  let int_of_2_uint8 bytes =
    let w = unsafe_get_uint16_le bytes 0 in
    if w < 0xd800 || 0xdfff < w then w else raise CodecError

  let int_of_4_uint8 bytes =
    let hi = unsafe_get_uint16_le bytes 0 in
    let lo = unsafe_get_uint16_le bytes 2 in
    if hi < 0xdc00 || hi > 0xdfff then raise CodecError
    else (((hi land 0x3ff) lsl 10) lor (lo land 0x3ff)) + 0x10000

  let from_bytes bytes =
    match Bytes.length bytes with
    | 2 -> int_of_2_uint8 bytes |> Uchar.unsafe_of_int
    | 4 -> int_of_4_uint8 bytes |> Uchar.unsafe_of_int
    | _ -> raise CodecError

  let width : ('a -> int -> char) -> 'a -> int -> int =
   fun f x pos ->
    let buf = Buffer.create 2 in
    Buffer.add_char buf @@ f x pos;
    Buffer.add_char buf @@ f x (pos + 1);
    let hi = unsafe_get_uint16_le (Buffer.to_bytes buf) 0 in
    Buffer.clear buf;
    if hi < 0xd800 || 0xdfff < hi then 2
    else if hi <= 0xdbff then 4
    else raise CodecError

  let width_rev f x pos =
    let decode_length_pos_1 fn v ofs buf =
      Buffer.add_char buf @@ fn v (ofs - 3);
      Buffer.add_char buf @@ fn v (ofs - 2);
      let hi = unsafe_get_uint16_le (Buffer.to_bytes buf) 0 in
      Buffer.clear buf;
      if hi <= 0xdbff then 4 else raise CodecError
    in
    let decode_length_pos_0 fn v ofs buf =
      Buffer.add_char buf @@ fn v (ofs - 1);
      Buffer.add_char buf @@ fn v ofs;
      let hi = unsafe_get_uint16_le (Buffer.to_bytes buf) 0 in
      Buffer.reset buf;
      if hi < 0xd800 || 0xdfff < hi then 2 else decode_length_pos_1 f v ofs buf
    in
    let buf = Buffer.create 2 in
    decode_length_pos_0 f x pos buf
end

module MakeCodec (S : Skel with type letter = Uchar.t) :
  T with type letter = Uchar.t = struct
  include S

  let version = Unicode.unicode_version
  let rep = Uchar.unsafe_of_int 0xfffd (* � *)
  let new_line = Uchar.unsafe_of_int 10
  let null = Uchar.unsafe_of_int 0x3fffffff
  let cgj_code = 0x034F (* combining grapheme joiner *)
  let cgj = Uchar.unsafe_of_int cgj_code

  (* let error_code = 0x1A0000 *)
  let max_width = 4
  let max_leading_nonstarters = 30 (* Safe-stream text *)
  let chunk_size = 32

  module Unsafe = struct
    let unsafe_slice s ofs len =
      if ofs = 0 && String.length s = len then Stdlib.Bytes.of_string s
      else
        let bytes = Stdlib.Bytes.create len in
        Stdlib.Bytes.unsafe_blit
          (Stdlib.Bytes.unsafe_of_string s)
          ofs bytes 0 len;
        bytes

    let unsafe_bytes_with_next_pos s ofs =
      let w = width String.unsafe_get s ofs in
      (unsafe_slice s ofs w, ofs + w)

    let unsafe_bytes_rev_with_next_pos s ofs =
      let w = width_rev String.unsafe_get s ofs in
      (unsafe_slice s (ofs + 1 - w) w, ofs - w)

    let unsafe_bytes s ofs =
      let w = width String.unsafe_get s ofs in
      unsafe_slice s ofs w

    let unsafe_bytes_rev s ofs =
      let w = width_rev String.unsafe_get s ofs in
      unsafe_slice s (ofs + 1 - w) w
  end

  type decomp = {
    leading_nonstarters : int;
    has_starter : bool;
    trailing_nonstarters : int;
    length : int;
  }

  let empty_d =
    {
      leading_nonstarters = 0;
      has_starter = false;
      trailing_nonstarters = 0;
      length = 0;
    }

  let add_decomp : decomp -> Uchar.t -> decomp =
   fun d u ->
    match Uunf.ccc u with
    | 0 -> { d with has_starter = true; length = succ d.length }
    | _ -> (
      match d.has_starter with
      | false ->
        {
          d with
          leading_nonstarters = succ d.leading_nonstarters;
          length = succ d.length;
        }
      | true ->
        {
          d with
          trailing_nonstarters = succ d.trailing_nonstarters;
          length = succ d.length;
        })

  (* TODO: this would be more efficient if Uunf was with bytes and not Uchar.t?*)
  type 'a enc = {
    enc_frm : Uunf.t;
    enc_nfkd : Uunf.t;
    enc_o_append : int -> 'a -> bytes -> 'a;
    mutable enc_index : int;
    enc_o : bytes;
    mutable enc_o_pos : int;
    enc_o_len : int;
    mutable enc_non_starters : int; (* number of nonstarters *)
  }

  let encoder_make : (int -> 'a -> bytes -> 'a) -> 'a enc =
   fun enc_o_append ->
    let enc_o_len = chunk_size * max_width in
    {
      enc_frm = Uunf.create S.form;
      enc_nfkd = Uunf.create (`NFKD :> Uunf.form);
      enc_o_append;
      enc_index = 0;
      enc_o = Bytes.make enc_o_len '\000';
      enc_o_pos = 0;
      enc_o_len;
      enc_non_starters = 0;
    }

  let empty_buffer enc acc0 =
    let rec iter acc bytes pos max =
      if pos >= max then acc
      else
        let w = width Bytes.get bytes pos in
        let acc = enc.enc_o_append enc.enc_index acc (Bytes.sub bytes pos w) in
        enc.enc_index <- enc.enc_index + 1;
        iter acc bytes (pos + w) max
    in
    iter acc0 (Bytes.sub enc.enc_o 0 enc.enc_o_pos) 0 enc.enc_o_pos

  let resize_buffer enc acc needed =
    let buf_len = enc.enc_o_len - enc.enc_o_pos in
    if buf_len < needed then empty_buffer enc acc else acc

  let add_output enc acc cp =
    (* TODO: this part is not optimized: 
       we calculate several times the length of t!*)
    let needed = S.byte_length cp in
    let acc = resize_buffer enc acc needed in
    let w = S.set enc.enc_o enc.enc_o_pos cp in
    enc.enc_o_pos <- enc.enc_o_pos + w;
    acc

  let rec normalize : 'a enc -> 'a -> Uunf.ret -> 'a =
   fun enc acc v ->
    let r = Uunf.add enc.enc_frm v in
    match r with
    | `Uchar u -> normalize enc (add_output enc acc u) `Await
    | `Await | `End -> acc

  let encoder_flush : 'a enc -> 'a -> 'a =
   fun enc acc -> normalize enc acc `End |> fun acc -> empty_buffer enc acc

  let rec add_nfkd : 'a enc -> decomp -> Uunf.ret -> decomp =
   fun enc d v ->
    match Uunf.add enc.enc_nfkd v with
    | `Uchar u -> add_nfkd enc (add_decomp d u) `Await
    | `Await | `End -> d

  let insert_cgj : 'a enc -> Uchar.t -> bool =
   fun enc u ->
    Uunf.reset enc.enc_nfkd;
    let d = add_nfkd enc (add_nfkd enc empty_d (`Uchar u)) `End in
    if enc.enc_non_starters + d.leading_nonstarters > max_leading_nonstarters
    then (
      if not d.has_starter then enc.enc_non_starters <- d.length
      else enc.enc_non_starters <- d.trailing_nonstarters;
      true)
    else (
      if not d.has_starter then
        enc.enc_non_starters <- enc.enc_non_starters + d.length
      else enc.enc_non_starters <- d.trailing_nonstarters;
      false)

  let encoder_add : type a. a enc -> a -> bytes -> a =
   fun enc acc bytes ->
    let u = from_bytes bytes in
    if stream_safe then
      if insert_cgj enc u then
        normalize enc (normalize enc acc (`Uchar cgj)) (`Uchar u)
      else normalize enc acc (`Uchar u)
    else normalize enc acc (`Uchar u)

  let quick_check = Unicode.nfx_quick_check form

  let is_encoded : letter -> bool =
   fun u ->
    match (quick_check u, Uunf.ccc u) with true, 0 -> true | _ -> false

  let to_seq s =
    let enc_frm = Uunf.create S.form in
    let enc_nfkd = Uunf.create (`NFKD :> Uunf.form) in
    let enc_non_starters = ref 0 in
    let tmp_uchar = ref None in
    let not_ended = ref true in
    let max_i = String.length s in
    let normalize v = Uunf.add enc_frm v in
    let rec add_nfkd d v =
      match Uunf.add enc_nfkd v with
      | `Uchar u -> add_nfkd (add_decomp d u) `Await
      | `Await | `End -> d
    in
    let insert_cgj u =
      Uunf.reset enc_nfkd;
      let d = add_nfkd (add_nfkd empty_d (`Uchar u)) `End in
      if !enc_non_starters + d.leading_nonstarters > max_leading_nonstarters
      then (
        if not d.has_starter then enc_non_starters := d.length
        else enc_non_starters := d.trailing_nonstarters;
        true)
      else (
        if not d.has_starter then
          enc_non_starters := !enc_non_starters + d.length
        else enc_non_starters := d.trailing_nonstarters;
        false)
    in
    let rec aux pos state () =
      match state with
      | `Uchar u -> Seq.Cons (u, aux pos `Await)
      | `Await -> (
        match normalize `Await with
        | `Uchar u -> aux pos (`Uchar u) ()
        | `Await -> (
          match !tmp_uchar with
          | Some u_src ->
            tmp_uchar := None;
            aux pos (normalize (`Uchar u_src)) ()
          | None ->
            let next_state, w =
              if pos >= max_i && !not_ended then (
                not_ended := false;
                (normalize `End, 0))
              else
                let w = width String.unsafe_get s pos in
                let u_src, w =
                  try (from_bytes @@ Unsafe.unsafe_slice s pos w, w)
                  with CodecError -> (rep, 1)
                in
                if stream_safe && insert_cgj u_src then (
                  tmp_uchar := Some u_src;
                  (normalize (`Uchar cgj), w))
                else (normalize (`Uchar u_src), w)
            in
            aux (pos + w) next_state ())
        | `End -> aux pos `End ())
      | `End -> Seq.Nil
    in
    aux 0 `Await

  let fold_left f acc0 s =
    let enc_frm = Uunf.create S.form in
    let enc_nfkd = Uunf.create (`NFKD :> Uunf.form) in
    let enc_non_starters = ref 0 in
    let tmp_uchar = ref None in
    let not_ended = ref true in
    let max_i = String.length s in
    let normalize v = Uunf.add enc_frm v in
    let rec add_nfkd d v =
      match Uunf.add enc_nfkd v with
      | `Uchar u -> add_nfkd (add_decomp d u) `Await
      | `Await | `End -> d
    in
    let insert_cgj u =
      Uunf.reset enc_nfkd;
      let d = add_nfkd (add_nfkd empty_d (`Uchar u)) `End in
      if !enc_non_starters + d.leading_nonstarters > max_leading_nonstarters
      then (
        if not d.has_starter then enc_non_starters := d.length
        else enc_non_starters := d.trailing_nonstarters;
        true)
      else (
        if not d.has_starter then
          enc_non_starters := !enc_non_starters + d.length
        else enc_non_starters := d.trailing_nonstarters;
        false)
    in
    let rec iter acc pos state =
      match state with
      | `Uchar u -> iter (f acc u) pos `Await
      | `Await -> (
        match normalize `Await with
        | `Uchar u -> iter acc pos (`Uchar u)
        | `Await -> (
          match !tmp_uchar with
          | Some u_src ->
            tmp_uchar := None;
            iter acc pos (normalize (`Uchar u_src))
          | None ->
            let next_state, w =
              if pos >= max_i && !not_ended then (
                not_ended := false;
                (normalize `End, 0))
              else
                let w = width String.unsafe_get s pos in
                let u_src, w =
                  try (from_bytes @@ Unsafe.unsafe_slice s pos w, w)
                  with CodecError -> (rep, 1)
                in
                if stream_safe && insert_cgj u_src then (
                  tmp_uchar := Some u_src;
                  (normalize (`Uchar cgj), w))
                else (normalize (`Uchar u_src), w)
            in
            iter acc (pos + w) next_state)
        | `End -> iter acc pos `End)
      | `End -> acc
    in
    iter acc0 0 `Await

  let to_list s = fold_left (fun acc x -> x :: acc) [] s |> List.rev
  let iter f s = fold_left (fun _ x -> f x) () s

  let fold_right f s acc0 =
    let enc_frm = Uunf.create S.form in
    let enc_nfkd = Uunf.create (`NFKD :> Uunf.form) in
    let enc_non_starters = ref 0 in
    let tmp_uchar = ref None in
    let not_ended = ref true in
    let max_i = String.length s in
    let normalize v = Uunf.add enc_frm v in
    let rec add_nfkd d v =
      match Uunf.add enc_nfkd v with
      | `Uchar u -> add_nfkd (add_decomp d u) `Await
      | `Await | `End -> d
    in
    let insert_cgj u =
      Uunf.reset enc_nfkd;
      let d = add_nfkd (add_nfkd empty_d (`Uchar u)) `End in
      if !enc_non_starters + d.leading_nonstarters > max_leading_nonstarters
      then (
        if not d.has_starter then enc_non_starters := d.length
        else enc_non_starters := d.trailing_nonstarters;
        true)
      else (
        if not d.has_starter then
          enc_non_starters := !enc_non_starters + d.length
        else enc_non_starters := d.trailing_nonstarters;
        false)
    in
    let rec iter acc pos state =
      match state with
      | `Uchar u -> iter (f u acc) pos `Await
      | `Await -> (
        match normalize `Await with
        | `Uchar u -> iter acc pos (`Uchar u)
        | `Await -> (
          match !tmp_uchar with
          | Some u_src ->
            tmp_uchar := None;
            iter acc pos (normalize (`Uchar u_src))
          | None ->
            let next_state, w =
              if pos < 0 && !not_ended then (
                not_ended := false;
                (normalize `End, 0))
              else
                let w = width_rev String.unsafe_get s pos in
                let u_src, w =
                  try (from_bytes @@ Unsafe.unsafe_slice s (pos + 1 - w) w, w)
                  with CodecError -> (rep, 1)
                in
                if stream_safe && insert_cgj u_src then (
                  tmp_uchar := Some u_src;
                  (normalize (`Uchar cgj), w))
                else (normalize (`Uchar u_src), w)
            in
            iter acc (pos - w) next_state)
        | `End -> iter acc pos `End)
      | `End -> acc
    in
    iter acc0 (max_i - 1) `Await
end

module Utf8 : T with type letter = Uchar.t =
  MakeCodec (MakeUtf8Skel (DefaultProperties))

module Utf16be : T with type letter = Uchar.t =
  MakeCodec (MakeUtf16beSkel (DefaultProperties))

module Utf16le : T with type letter = Uchar.t =
  MakeCodec (MakeUtf16leSkel (DefaultProperties))

module Latin1 : T with type letter = char = struct
  type letter = char

  let version = Sys.ocaml_version

  module Unsafe = struct
    let unsafe_slice : string -> int -> int -> bytes =
     fun s ofs len -> Bytes.sub (Bytes.unsafe_of_string s) ofs len

    let unsafe_bytes_with_next_pos : string -> int -> bytes * int =
     fun s ofs -> (unsafe_slice s ofs 1, succ ofs)

    let unsafe_bytes : string -> int -> bytes =
     fun s ofs -> Bytes.sub (Bytes.unsafe_of_string s) ofs 1

    let unsafe_bytes_rev_with_next_pos : string -> int -> bytes * int =
     fun s ofs -> (unsafe_bytes s ofs, pred ofs)

    let unsafe_bytes_rev : string -> int -> bytes =
     fun s ofs -> unsafe_bytes s ofs
  end

  let rep = Char.unsafe_chr 32
  let new_line = '\n'
  let null = '\000'
  let max_width = 1
  let byte_length = fun _ -> 1
  let equal = Char.equal
  let compare = Char.compare
  let from_int = fun i -> try Char.chr i with _ -> raise CodecError
  let to_int = Char.code
  let to_bytes = fun t -> Bytes.make 1 @@ t

  let from_bytes =
   fun bytes -> try Bytes.get bytes 0 with _ -> raise CodecError

  let set =
   fun bytes ofs t ->
    try
      Bytes.set bytes ofs t;
      1
    with _ -> raise CodecError

  let add = fun buf t -> try Buffer.add_char buf t with _ -> raise CodecError
  let width = fun _ _ _ -> 1
  let width_rev = fun _ _ _ -> 1

  (* encoder *)

  type 'a enc = 'a -> bytes -> 'a

  let encoder_make : (int -> 'a -> bytes -> 'a) -> 'a enc = fun f -> f 0
  let encoder_add : 'a enc -> 'a -> bytes -> 'a = fun enc x bytes -> enc x bytes
  let encoder_flush : 'a enc -> 'a -> 'a = fun _enc x -> x
  let is_encoded : letter -> bool = fun _ -> true
  let to_seq = String.to_seq
  let iter = String.iter
  let fold_left = String.fold_left
  let fold_right = String.fold_right
  let to_list s = String.fold_left (fun acc c -> c :: acc) [] s |> List.rev

  (*pretty printer *)
  let pp : Format.formatter -> char -> unit =
   fun ppf t -> Format.fprintf ppf "%c" t

  let dump : Format.formatter -> char -> unit =
   fun ppf t -> Format.fprintf ppf "{%02X}" (Char.code t)
end
