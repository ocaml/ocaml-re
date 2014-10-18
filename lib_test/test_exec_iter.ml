#!/usr/bin/env ocaml
#use "topfind";;
#require "sequence";;
#directory "_build/lib";;
#load "re.cma";;
#load "re_posix.cma";;
let re = Re_posix.re "a*(ab)" |> Re.compile;;
let s = "aaaaabaaa aaabaaabaaaabaa axbaaba";;
Re.exec_iter re s
  |> Sequence.iter
    (fun s ->
      let i,j = Re.get_ofs s 0 in
      Printf.printf "%s at %d,%d\n" (Re.get s 0) i j
    );;

