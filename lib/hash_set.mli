(** A specialized hash table that makes the following trade-offs:
    - Open addresing. Bucketing is quite memory intensive and dune is already
      a memory hog.
    - No boxing for empty slots. We make use of the fact that id's are never
      negative to achieve this.
    - No saving of the hash. Recomputing the hash for id's is a no-op. *)

type t

val create : unit -> t
val is_empty : t -> bool
val add : t -> int -> unit
val mem : t -> int -> bool
val clear : t -> unit
val pp : t Fmt.t
