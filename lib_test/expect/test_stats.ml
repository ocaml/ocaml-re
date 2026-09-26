open Import
module Stats = Re_private.Stats

let alphabet = String.init 256 Char.chr

let print_stats name re =
  let { Stats.colors; states } = Re.stats re in
  printf "%s, colors=%d, states=%d\n" name colors states
;;

let report name pattern inputs =
  let re = Re.compile pattern in
  List.iter inputs ~f:(fun input -> ignore (Re.execp re input : bool));
  print_stats name re
;;

let%expect_test "compiled automata report their colors and interned states" =
  let empty_groups = Stdlib.List.init 4 (fun _ -> Re.group Re.epsilon) in
  let bytes = Stdlib.List.init 256 (fun b -> String.make 1 (Char.chr b)) in
  report "literal" (Re.str "abc") [ "abc" ];
  report "wide literal" (Re.str alphabet) [ alphabet ];
  report "no assertion loop" (Re.rep (Re.char 'c')) [ String.make 10 'c' ];
  report
    "word boundaries"
    Re.(seq [ bow; group (rep1 wordc); eow ])
    [ "the quick"; "a"; "" ];
  report "wide empty match" (Re.alt [ Re.seq empty_groups; Re.str alphabet ]) bytes;
  [%expect
    {|
    literal, colors=4, states=4
    wide literal, colors=256, states=257
    no assertion loop, colors=2, states=3
    word boundaries, colors=2, states=4
    wide empty match, colors=256, states=4
    |}]
;;

let%expect_test "state counts only cover interned states" =
  let re = Re.compile Re.(alt [ str "ab"; str "cd" ]) in
  print_stats "fresh" re;
  ignore (Re.execp re "ab" : bool);
  print_stats "after \"ab\"" re;
  ignore (Re.execp re "cd" : bool);
  print_stats "after \"cd\"" re;
  [%expect
    {|
    fresh, colors=5, states=0
    after "ab", colors=5, states=3
    after "cd", colors=5, states=4
    |}]
;;

let%expect_test "repeated and complemented bytes reuse colors" =
  report "repeated byte" (Re.str "aaa") [ "aaa" ];
  report "complemented byte" Re.(alt [ char 'b'; compl [ char 'b' ] ]) [ "b"; "c" ];
  [%expect
    {|
    repeated byte, colors=2, states=4
    complemented byte, colors=1, states=2
    |}]
;;

let%expect_test "case-insensitive literals share colors" =
  report "no case" (Re.no_case (Re.str "aB")) [ "ab"; "AB" ];
  [%expect {| no case, colors=3, states=3 |}]
;;

let%expect_test "boundary and anchor categories" =
  let inputs = [ ""; "abc"; "xabc"; "abc\n"; "abc\ndef"; "x abc x" ] in
  report "bol/eol" Re.(seq [ bol; str "abc"; eol ]) inputs;
  report "multiline" (Re.Perl.re ~opts:[ `Multiline ] "^abc$") inputs;
  report "leol" Re.(seq [ str "abc"; leol ]) inputs;
  report "not_boundary" Re.(seq [ not_boundary; str "ab" ]) [ "ab"; "xab"; "ab x" ];
  report "bos/eos" Re.(seq [ bos; str "abc"; eos ]) inputs;
  report "anchored" (Re.Perl.re ~opts:[ `Anchored ] "abc") inputs;
  [%expect
    {|
    bol/eol, colors=5, states=6
    multiline, colors=5, states=6
    leol, colors=5, states=6
    not_boundary, colors=4, states=5
    bos/eos, colors=4, states=5
    anchored, colors=4, states=6
    |}]
;;

let%expect_test "equivalent descriptors share states" =
  let inputs = [ "ab"; "cb"; "b"; "a"; "abc" ] in
  report "common suffix" Re.(alt [ str "ab"; str "cb" ]) inputs;
  report "suffix branch" Re.(alt [ str "ab"; str "b" ]) inputs;
  report "prefix branch" Re.(alt [ str "a"; str "ab" ]) inputs;
  [%expect
    {|
    common suffix, colors=4, states=6
    suffix branch, colors=3, states=6
    prefix branch, colors=3, states=4
    |}]
;;

let%expect_test "capture groups" =
  let inputs = [ "ab"; "cb"; "b"; "a"; "abc" ] in
  report "captured alternation" Re.(group (alt [ str "ab"; str "cb" ])) inputs;
  report "plain alternation" Re.(alt [ str "ab"; str "cb" ]) inputs;
  report "nested groups" Re.(group (group (str "ab"))) [ "ab"; "abc" ];
  report "plain sequence" Re.(str "ab") [ "ab"; "abc" ];
  report "optional group" Re.(seq [ str "a"; group (opt (str "b")) ]) [ "a"; "ab" ];
  report "plain optional" Re.(seq [ str "a"; opt (str "b") ]) [ "a"; "ab" ];
  report
    "repeated group"
    Re.(seq [ rep1 (group (rep1 wordc)); char '!' ])
    [ "ab!cd!"; "x!" ];
  report "plain repeat" Re.(seq [ rep1 wordc; char '!' ]) [ "ab!cd!"; "x!" ];
  [%expect
    {|
    captured alternation, colors=4, states=6
    plain alternation, colors=4, states=6
    nested groups, colors=3, states=4
    plain sequence, colors=3, states=4
    optional group, colors=3, states=3
    plain optional, colors=3, states=3
    repeated group, colors=3, states=5
    plain repeat, colors=3, states=5
    |}]
;;

let%expect_test "nullable repetitions" =
  let inputs = [ ""; "a"; "aaa"; "b" ] in
  report "optional loop" Re.(rep (opt (char 'a'))) inputs;
  report "nested loop" Re.(rep (rep (char 'a'))) inputs;
  report "empty group loop" Re.(rep (group epsilon)) [ ""; "a" ];
  [%expect
    {|
    optional loop, colors=2, states=4
    nested loop, colors=2, states=4
    empty group loop, colors=1, states=2
    |}]
;;

let%expect_test "greedy and lazy repetition" =
  let inputs = [ "axxb"; "ab"; "xxaxxbxx" ] in
  report "greedy" Re.(seq [ char 'a'; rep any; char 'b' ]) inputs;
  report "non-greedy" (Re.Perl.re "a.*?b") inputs;
  report
    "shortest"
    Re.(shortest (seq [ bos; group (rep (str "a")); group (rep (str "a")); eos ]))
    [ "aa" ];
  [%expect
    {|
    greedy, colors=3, states=7
    non-greedy, colors=4, states=6
    shortest, colors=2, states=3
    |}]
;;

let%expect_test "character class algebra and folding" =
  let inputs = [ "x"; "5"; "aeiou"; "A"; "-" ] in
  report "difference" Re.(diff (rg 'a' 'z') (set "aeiou")) inputs;
  report "intersection" Re.(inter [ wordc; compl [ digit ] ]) inputs;
  report "folded range" Re.(no_case (rg 'a' 'z')) [ "q"; "Q" ];
  report "dotall" (Re.Pcre.re ~flags:[ `DOTALL ] "a.b") [ "a\nb" ];
  [%expect
    {|
    difference, colors=2, states=3
    intersection, colors=2, states=4
    folded range, colors=2, states=2
    dotall, colors=3, states=4
    |}]
;;

let%expect_test "partial and streaming matches" =
  let re = Re.compile (Re.str "abcdef") in
  let partial input =
    (match Re.exec_partial_detailed re input with
     | `Mismatch -> Printf.printf "mismatch\n"
     | `Partial pos -> Printf.printf "partial %d\n" pos
     | `Full _ -> Printf.printf "full\n");
    print_stats (Printf.sprintf "after %S" input) re
  in
  partial "abc";
  partial "abcd";
  let stream_re = Re.compile Re.(seq [ str "ab"; str "cd" ]) in
  print_stats "stream fresh" stream_re;
  let st = Re.Stream.create stream_re in
  let st =
    match Re.Stream.feed st "ab" ~pos:0 ~len:2 with
    | Re.Stream.Ok st -> st
    | No_match -> failwith "unexpected No_match"
  in
  print_stats "stream after \"ab\"" stream_re;
  ignore (Re.Stream.finalize st "cd" ~pos:0 ~len:2 : bool);
  print_stats "stream after \"cd\"" stream_re;
  [%expect
    {|
    partial 0
    after "abc", colors=7, states=4
    partial 0
    after "abcd", colors=7, states=5
    stream fresh, colors=5, states=0
    stream after "ab", colors=5, states=3
    stream after "cd", colors=5, states=5
    |}]
;;

let%expect_test "marks do not change state identity" =
  let marked r = snd (Re.mark r) in
  let inputs = [ "ab"; "abcd"; "xab"; "abab" ] in
  report "mark literal" (marked Re.(seq [ str "ab"; opt (str "cd") ])) inputs;
  report "mark in loop" (marked Re.(rep1 (snd (mark (str "ab"))))) inputs;
  report
    "marked branches"
    (Re.alt (Stdlib.List.map marked [ Re.str "ab"; Re.str "cd"; Re.str "ef" ]))
    inputs;
  report "plain branches" Re.(alt [ str "ab"; str "cd"; str "ef" ]) inputs;
  report
    "marked duplicates"
    (Re.alt (Stdlib.List.map marked [ Re.str "ab"; Re.str "ab" ]))
    inputs;
  report "plain duplicates" Re.(alt [ str "ab"; str "ab" ]) inputs;
  [%expect
    {|
    mark literal, colors=5, states=7
    mark in loop, colors=3, states=7
    marked branches, colors=7, states=5
    plain branches, colors=7, states=5
    marked duplicates, colors=3, states=5
    plain duplicates, colors=3, states=5
    |}]
;;
