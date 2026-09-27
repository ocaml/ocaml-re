(** Statistics about the automaton of a compiled regular expression.

    Internal to the library, its tests and its benchmarks. Not part of the
    stable public interface. *)
type t =
  { colors : int
  ; states : int
  }
