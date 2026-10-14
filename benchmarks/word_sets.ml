open Import
open Workload
(* Word-set regexes: large alternations of literals, the shape used by
   spell-checkers, keyword filters and word lists. The word list is
   dwyl/english-words, commit 20f5cc9b3f0ccc8ce45d814c532b7c2031bba31c
   (Unlicense, public domain), filtered to 2-15 letter lowercase words and
   sampled evenly to 5,000 entries. The smaller cases are nested subsets so
   state, color and forcing costs can be compared directly.

   Samples only establish polarity; the workloads care about compile time,
   colors, states and forcing. *)

let words = Stdio.In_channel.read_lines "benchmarks/words/english-5000.txt"
let every n words = List.filteri (fun i _ -> i mod n = 0) words

let dictionary name words =
  generated
    name
    ~pattern:(fun () -> Re.alt (List.map Re.str words))
    ~samples:(fun () -> [ yes (List.hd words); no "0123456789" ])
;;

let dictionary_with_boundaries name words =
  generated
    name
    ~pattern:(fun () -> Re.word (Re.alt (List.map Re.str words)))
    ~samples:(fun () -> [ yes (List.hd words); no "0123456789" ])
;;

let dictionary_whole_string name words =
  generated
    name
    ~pattern:(fun () -> Re.whole_string (Re.alt (List.map Re.str words)))
    ~samples:(fun () -> [ yes (List.hd words); no "0123456789" ])
;;

(* Unanchored word sets blow up superlinearly once the start position is free:
   500 words already force to ~20k states, and 2,500 words take ~10 s. The
   unanchored cases therefore stay at 500 words or fewer. The larger dictionary
   cases are whole-string or word-boundary, which force to a trie-like
   automaton. *)
let cases =
  [ dictionary "words/dictionary/250" (every 20 words)
  ; dictionary "words/dictionary/500" (every 10 words)
  ; dictionary_whole_string "words/dictionary/1000/whole-string" (every 5 words)
  ; dictionary_whole_string "words/dictionary/2500/whole-string" (every 2 words)
  ; dictionary_whole_string "words/dictionary/5000/whole-string" words
  ; dictionary_with_boundaries "words/dictionary/5000/word-boundary" words
  ]
;;
