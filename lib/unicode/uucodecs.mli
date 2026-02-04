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

module DefaultProperties : Properties

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

module MakeUtf8Skel (_ : Properties) : Skel with type letter = Uchar.t
module MakeUtf16beSkel (_ : Properties) : Skel with type letter = Uchar.t
module MakeUtf16leSkel (_ : Properties) : Skel with type letter = Uchar.t

module MakeCodec (_ : Skel with type letter = Uchar.t) :
  T with type letter = Uchar.t

module Utf8 : T with type letter = Uchar.t
module Utf16be : T with type letter = Uchar.t
module Utf16le : T with type letter = Uchar.t
module Latin1 : T with type letter = Char.t