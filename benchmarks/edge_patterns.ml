open Pattern

(* Pathological repetition shapes. These are deliberately not common patterns:
   the body of an outer +/* contains further +/* (and often capture groups),
   which is the shape that stresses derivative construction. They are
   construction-cost edge cases, not examples of real-world regexes. Forcing
   them is cheap (at most 305 states and a few milliseconds), so they take part
   in the forced-state snapshots. *)

let nested_seq_of_reps =
  perl
    "edge/nested-repetition/seq-of-reps"
    {|^((a*b*c*d*)(e*f*g*h*))+$|}
    [ ""; "b"; "h"; "abc"; "abcdefgh"; "aabbccddeeffgghh"; "d"; "efgh" ]
    [ "x"; "abcx"; "a0"; "abcdefghx" ]
;;

let nested_capture_stars =
  perl
    "edge/nested-repetition/capture-stars"
    {|^((((a*)(b*))*)((c*)(d*)))+$|}
    [ ""; "abcd"; "aabbccdd"; "badc"; "dddd" ]
    [ "x"; "ax"; "a1"; "abx" ]
;;

let nested_star_plus =
  perl
    "edge/nested-repetition/star-plus"
    {|^((a*)*b)+$|}
    [ "b"; "ab"; "aab"; "bb"; "aaabbb" ]
    [ ""; "a"; "ba"; "c"; "ac" ]
;;

let nested_alt_of_reps =
  perl
    "edge/nested-repetition/alt-of-reps"
    {|^((a+|b+)(c+|d+))*$|}
    [ ""; "ac"; "bd"; "ad"; "bc"; "aaaccc"; "acbd"; "adbc" ]
    [ "ab"; "cd"; "ca"; "x"; "acb" ]
;;

let nested_flat_reps =
  perl
    "edge/nested-repetition/flat-reps"
    {|^((a+)(b+)(c+))+$|}
    [ "abc"; "aaabbbccc"; "abcabc"; "aabbcc" ]
    [ ""; "ab"; "acb"; "aabbccd"; "bc" ]
;;

let nested_word_digit =
  perl
    "edge/nested-repetition/word-digit"
    {|^([a-z]+[0-9]+)+$|}
    [ "a1"; "abc123"; "a1b2"; "abc123def456" ]
    [ ""; "1a"; "a"; "1"; "a1b"; "a1b2c"; "A1" ]
;;

let nested_alt_literals =
  perl
    "edge/nested-repetition/alt-literals"
    {|^((ab|ac|ad)(xy|xz))+$|}
    [ "abxy"; "acxz"; "adxyabxz"; "abxyabxy" ]
    [ ""; "ab"; "xyab"; "abx"; "abxyab" ]
;;

let nested_single_rep =
  perl
    "edge/nested-repetition/single-rep"
    {|^(a*b)+$|}
    [ "b"; "ab"; "aabbb"; "abab" ]
    [ ""; "a"; "ba"; "aba"; "c" ]
;;

let cases =
  [ nested_seq_of_reps
  ; nested_capture_stars
  ; nested_star_plus
  ; nested_alt_of_reps
  ; nested_flat_reps
  ; nested_word_digit
  ; nested_alt_literals
  ; nested_single_rep
  ]
;;

let%test_unit "edge pattern samples match with the intended polarity" =
  check_samples cases
;;
