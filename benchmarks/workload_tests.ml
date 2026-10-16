open Import

let%expect_test "unique workload names" =
  let names = List.map (fun (c : Workload.case) -> c.name) Suite.cases in
  assert (List.length names = List.length (List.sort_uniq String.compare names));
  print_endline "unique";
  [%expect {| unique |}]
;;

let cases = List.filter (fun (c : Workload.case) -> c.runtest) Suite.cases

let check_workload (c : Workload.case) =
  let t = c.make () in
  assert (Option.is_some c.construction = Option.is_some t.construct);
  assert (c.modes = List.map fst t.runs);
  assert (c.force = t.force);
  let check_pattern pattern =
    let re = Re.compile pattern in
    t.check re;
    List.iter
      (fun (_, run) ->
         run re;
         run re)
      t.runs;
    t.check re;
    (* copy_re must start cold even when copied from a warm regex. *)
    let cold = Re.copy_re re in
    assert ((Re.stats cold).states = 0);
    t.check cold
  in
  check_pattern t.pattern;
  Option.iter
    (fun construct ->
       let pattern = construct () in
       (* Parser-only workloads can have few useful matching samples, so also
          check that the timed parser constructs the advertised expression. *)
       if Format.asprintf "%a" Re.pp pattern <> Format.asprintf "%a" Re.pp t.pattern
       then failwith (c.name ^ ": constructor changed pattern");
       check_pattern pattern)
    t.construct
;;

let%test_unit "workload behavior and metadata" = List.iter check_workload cases

let%test_unit "constructors defer initialization" =
  let built = ref 0 in
  let cases =
    [ Workload.inputs "inputs" (fun () ->
        incr built;
        Re.char 'a', [ Workload.yes "a"; Workload.no "b" ])
    ; Workload.perl "parser" (fun () ->
        incr built;
        "a", [ Workload.yes "a"; Workload.no "b" ])
    ; Workload.custom "custom" (fun () ->
        incr built;
        ( Re.char 'a'
        , (fun re -> ignore (Re.execp re "a"))
        , fun re -> assert (Re.execp re "a") ))
    ]
  in
  assert (!built = 0);
  List.iter check_workload cases;
  assert (!built = 3)
;;

let%test_unit "generated phases rebuild the AST but not the inputs" =
  let builds = ref 0 in
  let samples = ref 0 in
  let c =
    Workload.generated
      "generated"
      ~pattern:(fun () ->
        incr builds;
        Re.char 'a')
      ~samples:(fun () ->
        incr samples;
        [ Workload.yes "a" ])
  in
  assert (!builds = 0 && !samples = 0);
  assert (c.construction = Some Workload.Build);
  let t = c.make () in
  assert (!builds = 1 && !samples = 1);
  let construct = Option.get t.construct in
  t.check (Re.compile (construct ()));
  t.check (Re.compile (construct ()));
  assert (!builds = 3 && !samples = 1)
;;

let%test_unit "checks reject incorrect sample expectations" =
  List.iter
    (fun sample ->
       let c = Workload.inputs "incorrect sample" (fun () -> Re.char 'a', [ sample ]) in
       let t = c.make () in
       match t.check (Re.compile t.pattern) with
       | () -> failwith "incorrect expectation was accepted"
       | exception Failure _ -> ())
    [ Workload.yes "b"; Workload.no "a" ]
;;

let%test_unit "checks reject a mismatched parser" =
  let c = Workload.perl "incorrect parser" (fun () -> "a", [ Workload.no "" ]) in
  let c =
    { c with
      make =
        (fun () ->
          let t = c.make () in
          { t with construct = Some (fun () -> Re.char 'b') })
    }
  in
  match check_workload c with
  | () -> failwith "incorrect parser was accepted"
  | exception Failure message ->
    assert (message = "incorrect parser: constructor changed pattern")
;;

let%test_module "consolidated workload statistics" =
  (module struct
    let () =
      if Sys.ocaml_version = "5.4.1" && Sys.word_size = 64
      then
        let module Tests = struct
          let%expect_test "colors, states and size after forcing" =
            List.iter
              (fun (c : Workload.case) -> Workload.report c.name (c.make ()))
              cases;
            [%expect
              {|
              20 zeroes:
                colors: 2
                states: 23
                compiled_words: 495
                forced_words: 3,860
                compile_alloc_words: 1,490
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              lots of a's:
                colors: 3
                states: 9
                compiled_words: 311
                forced_words: 905
                compile_alloc_words: 1,054
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              media type match:
                colors: 3
                states: 10
                compiled_words: 320
                forced_words: 1,088
                compile_alloc_words: 1,139
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri:
                colors: 6
                states: 242
                compiled_words: 745
                forced_words: 36,760
                compile_alloc_words: 2,858
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              tex gitignore:
                colors: 42
                states: 68
                compiled_words: 11,363
                forced_words: 81,986
                compile_alloc_words: 51,761
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/no group:
                colors: 13
                states: 557
                compiled_words: 962
                forced_words: 43,708
                compile_alloc_words: 4,004
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/group:
                colors: 13
                states: 374
                compiled_words: 842
                forced_words: 58,104
                compile_alloc_words: 3,882
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              http/auto/execp no group:
                colors: 13
                states: 799
                compiled_words: 1,501
                forced_words: 97,766
                compile_alloc_words: 4,572
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/auto/all_gen:
                colors: 13
                states: 707
                compiled_words: 1,281
                forced_words: 121,887
                compile_alloc_words: 4,350
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              string traversal from #210:
                colors: 3
                states: 65
                compiled_words: 465
                forced_words: 7,862
                compile_alloc_words: 1,440
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              kleene star compilation:
                colors: 2
                states: 5
                compiled_words: 274
                forced_words: 599
                compile_alloc_words: 905
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1:
                colors: 3
                states: 1,003
                compiled_words: 10,294
                forced_words: 2,061,344
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2:
                colors: 4
                states: 1,003
                compiled_words: 10,312
                forced_words: 4,564,601
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              repeated sequence re:
                colors: 256
                states: 12,803
                compiled_words: 130,329
                forced_words: 8,906,443
                compile_alloc_words: 157,964
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              split on whitespace:
                colors: 2
                states: 5
                compiled_words: 284
                forced_words: 650
                compile_alloc_words: 955
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              shared prefixes:
                colors: 27
                states: 8
                compiled_words: 6,208
                forced_words: 11,300
                compile_alloc_words: 24,997
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/1:
                colors: 6
                states: 242
                compiled_words: 745
                forced_words: 36,760
                compile_alloc_words: 2,858
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/2:
                colors: 6
                states: 242
                compiled_words: 745
                forced_words: 36,760
                compile_alloc_words: 2,858
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/3:
                colors: 6
                states: 242
                compiled_words: 745
                forced_words: 36,760
                compile_alloc_words: 2,858
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/email-html5:
                colors: 7
                states: 256
                compiled_words: 3,352
                forced_words: 28,680
                compile_alloc_words: 8,900
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/url:
                colors: 12
                states: 17
                compiled_words: 520
                forced_words: 2,056
                compile_alloc_words: 2,325
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv4:
                colors: 8
                states: 35
                compiled_words: 970
                forced_words: 5,006
                compile_alloc_words: 3,244
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv6:
                colors: 16
                states: 1,244
                compiled_words: 11,204
                forced_words: 258,973
                compile_alloc_words: 23,244
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/mac:
                colors: 4
                states: 21
                compiled_words: 564
                forced_words: 2,429
                compile_alloc_words: 1,532
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-date:
                colors: 8
                states: 17
                compiled_words: 540
                forced_words: 1,973
                compile_alloc_words: 2,065
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/time-24h:
                colors: 8
                states: 13
                compiled_words: 422
                forced_words: 1,427
                compile_alloc_words: 1,759
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-timestamp:
                colors: 9
                states: 31
                compiled_words: 670
                forced_words: 3,216
                compile_alloc_words: 2,815
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/phone-us:
                colors: 8
                states: 35
                compiled_words: 580
                forced_words: 3,567
                compile_alloc_words: 2,309
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/credit-card:
                colors: 11
                states: 66
                compiled_words: 1,015
                forced_words: 6,277
                compile_alloc_words: 2,972
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/uuid:
                colors: 6
                states: 40
                compiled_words: 654
                forced_words: 3,833
                compile_alloc_words: 2,100
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/semver:
                colors: 8
                states: 41
                compiled_words: 927
                forced_words: 5,230
                compile_alloc_words: 4,281
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/base64:
                colors: 4
                states: 12
                compiled_words: 417
                forced_words: 1,397
                compile_alloc_words: 1,742
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/domain:
                colors: 6
                states: 316
                compiled_words: 4,795
                forced_words: 37,852
                compile_alloc_words: 8,066
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/apache-combined:
                colors: 8
                states: 82
                compiled_words: 964
                forced_words: 8,603
                compile_alloc_words: 3,312
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/syslog-rfc3164:
                colors: 13
                states: 33
                compiled_words: 786
                forced_words: 3,319
                compile_alloc_words: 3,111
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/structured-line:
                colors: 13
                states: 37
                compiled_words: 850
                forced_words: 3,292
                compile_alloc_words: 3,293
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/csv/rfc4180-row:
                colors: 5
                states: 16
                compiled_words: 485
                forced_words: 1,961
                compile_alloc_words: 2,104
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/string:
                colors: 8
                states: 12
                compiled_words: 439
                forced_words: 1,471
                compile_alloc_words: 1,866
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/number:
                colors: 8
                states: 14
                compiled_words: 499
                forced_words: 1,659
                compile_alloc_words: 2,077
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/tag:
                colors: 7
                states: 26
                compiled_words: 499
                forced_words: 2,807
                compile_alloc_words: 1,999
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/comment:
                colors: 5
                states: 15
                compiled_words: 376
                forced_words: 1,630
                compile_alloc_words: 1,353
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/markdown/link:
                colors: 7
                states: 17
                compiled_words: 575
                forced_words: 1,530
                compile_alloc_words: 2,335
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/comment/c-style:
                colors: 3
                states: 27
                compiled_words: 430
                forced_words: 3,005
                compile_alloc_words: 1,663
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/keywords:
                colors: 26
                states: 225
                compiled_words: 3,808
                forced_words: 27,179
                compile_alloc_words: 16,565
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/lexer:
                colors: 19
                states: 246
                compiled_words: 1,123
                forced_words: 35,566
                compile_alloc_words: 6,797
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/number-literals:
                colors: 15
                states: 27
                compiled_words: 791
                forced_words: 3,388
                compile_alloc_words: 4,322
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/number/roman:
                colors: 9
                states: 30
                compiled_words: 821
                forced_words: 3,420
                compile_alloc_words: 2,666
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/money/usd:
                colors: 6
                states: 15
                compiled_words: 443
                forced_words: 1,645
                compile_alloc_words: 1,532
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/docker/reference:
                colors: 18
                states: 344
                compiled_words: 4,462
                forced_words: 43,146
                compile_alloc_words: 12,105
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/aws-access-key:
                colors: 9
                states: 5,473
                compiled_words: 612
                forced_words: 610,951
                compile_alloc_words: 2,287
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/api-key-line:
                colors: 18
                states: 893
                compiled_words: 844
                forced_words: 95,724
                compile_alloc_words: 4,989
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/youtube/video-id:
                colors: 21
                states: 294
                compiled_words: 961
                forced_words: 33,080
                compile_alloc_words: 4,011
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/date/us-slash:
                colors: 8
                states: 95
                compiled_words: 652
                forced_words: 10,710
                compile_alloc_words: 2,838
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/seq-of-reps:
                colors: 10
                states: 83
                compiled_words: 724
                forced_words: 13,400
                compile_alloc_words: 2,127
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/capture-stars:
                colors: 6
                states: 305
                compiled_words: 789
                forced_words: 59,225
                compile_alloc_words: 1,826
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/star-plus:
                colors: 4
                states: 21
                compiled_words: 427
                forced_words: 2,308
                compile_alloc_words: 1,134
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-of-reps:
                colors: 6
                states: 22
                compiled_words: 475
                forced_words: 2,662
                compile_alloc_words: 1,446
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/flat-reps:
                colors: 5
                states: 17
                compiled_words: 617
                forced_words: 2,283
                compile_alloc_words: 1,443
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/word-digit:
                colors: 4
                states: 11
                compiled_words: 423
                forced_words: 1,251
                compile_alloc_words: 1,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-literals:
                colors: 9
                states: 13
                compiled_words: 592
                forced_words: 1,770
                compile_alloc_words: 1,856
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/single-rep:
                colors: 4
                states: 13
                compiled_words: 369
                forced_words: 1,399
                compile_alloc_words: 1,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/whitespace/trim:
                colors: 3
                states: 8
                compiled_words: 341
                forced_words: 1,067
                compile_alloc_words: 1,237
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/whitespace/run:
                colors: 2
                states: 5
                compiled_words: 284
                forced_words: 650
                compile_alloc_words: 969
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/list/tab-separated:
                colors: 3
                states: 47
                compiled_words: 413
                forced_words: 5,236
                compile_alloc_words: 1,245
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/list/separator-run:
                colors: 3
                states: 5
                compiled_words: 292
                forced_words: 662
                compile_alloc_words: 1,140
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/line/crlf:
                colors: 3
                states: 6
                compiled_words: 299
                forced_words: 810
                compile_alloc_words: 1,039
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/line/blank:
                colors: 3
                states: 9
                compiled_words: 298
                forced_words: 1,004
                compile_alloc_words: 1,125
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/comment/hash:
                colors: 4
                states: 11
                compiled_words: 335
                forced_words: 1,286
                compile_alloc_words: 1,333
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/integer:
                colors: 3
                states: 7
                compiled_words: 316
                forced_words: 864
                compile_alloc_words: 1,127
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/float:
                colors: 3
                states: 10
                compiled_words: 407
                forced_words: 1,378
                compile_alloc_words: 1,393
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/hex:
                colors: 4
                states: 8
                compiled_words: 320
                forced_words: 878
                compile_alloc_words: 1,196
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/config/key-value:
                colors: 4
                states: 13
                compiled_words: 431
                forced_words: 1,799
                compile_alloc_words: 1,719
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/config/colon-value:
                colors: 4
                states: 15
                compiled_words: 396
                forced_words: 2,257
                compile_alloc_words: 1,495
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/string/double-quoted:
                colors: 2
                states: 9
                compiled_words: 328
                forced_words: 1,063
                compile_alloc_words: 1,105
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/string/single-quoted:
                colors: 2
                states: 9
                compiled_words: 328
                forced_words: 1,063
                compile_alloc_words: 1,105
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/unit/duration:
                colors: 8
                states: 8
                compiled_words: 449
                forced_words: 1,161
                compile_alloc_words: 1,669
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/version/dotted:
                colors: 3
                states: 13
                compiled_words: 482
                forced_words: 1,708
                compile_alloc_words: 1,576
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/terminal/ansi:
                colors: 5
                states: 7
                compiled_words: 328
                forced_words: 847
                compile_alloc_words: 1,243
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/html/entity:
                colors: 9
                states: 10
                compiled_words: 430
                forced_words: 1,306
                compile_alloc_words: 1,690
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/template/opam:
                colors: 4
                states: 10
                compiled_words: 350
                forced_words: 1,147
                compile_alloc_words: 1,257
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/template/mustache:
                colors: 3
                states: 11
                compiled_words: 344
                forced_words: 1,257
                compile_alloc_words: 1,193
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/adjacent:
                colors: 8
                states: 8
                compiled_words: 369
                forced_words: 916
                compile_alloc_words: 1,445
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/interleaved:
                colors: 5
                states: 6
                compiled_words: 368
                forced_words: 760
                compile_alloc_words: 1,214
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/4:
                colors: 11
                states: 11
                compiled_words: 837
                forced_words: 1,976
                compile_alloc_words: 2,864
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/16:
                colors: 12
                states: 26
                compiled_words: 2,283
                forced_words: 7,672
                compile_alloc_words: 7,402
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/log files:
                colors: 10
                states: 30
                compiled_words: 1,119
                forced_words: 4,025
                compile_alloc_words: 3,298
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/escape tokens:
                colors: 10
                states: 16
                compiled_words: 627
                forced_words: 2,025
                compile_alloc_words: 2,402
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/routes:
                colors: 21
                states: 65
                compiled_words: 1,275
                forced_words: 8,481
                compile_alloc_words: 4,422
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1/10:
                colors: 3
                states: 11
                compiled_words: 10,294
                forced_words: 11,228
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/20:
                colors: 3
                states: 21
                compiled_words: 10,294
                forced_words: 12,248
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/40:
                colors: 3
                states: 41
                compiled_words: 10,294
                forced_words: 15,488
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/80:
                colors: 3
                states: 81
                compiled_words: 10,294
                forced_words: 26,768
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/100:
                colors: 3
                states: 101
                compiled_words: 10,294
                forced_words: 34,808
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/1000:
                colors: 3
                states: 1,001
                compiled_words: 10,294
                forced_words: 2,052,992
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/10:
                colors: 4
                states: 11
                compiled_words: 10,312
                forced_words: 11,438
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/20:
                colors: 4
                states: 21
                compiled_words: 10,312
                forced_words: 13,143
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/40:
                colors: 4
                states: 41
                compiled_words: 10,312
                forced_words: 19,253
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/80:
                colors: 4
                states: 81
                compiled_words: 10,312
                forced_words: 42,273
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/100:
                colors: 4
                states: 101
                compiled_words: 10,312
                forced_words: 59,183
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/1000:
                colors: 4
                states: 1,001
                compiled_words: 10,312
                forced_words: 4,546,517
                compile_alloc_words: 11,187
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              expression IDs/broad/16/1024:
                colors: 9
                states: 1,033
                compiled_words: 11,057
                forced_words: 92,589
                compile_alloc_words: 14,071
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 801
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/broad/4096/16384:
                colors: 17
                states: 16,401
                compiled_words: 350,490
                forced_words: 2,293,398
                compile_alloc_words: 1,248,167
                compile_major_words: 407,092
                compile_promoted_words: 407,092
                construct_alloc_words: 500,327
                construct_major_words: 141,345
                construct_promoted_words: 141,345
                forcing: full
              expression IDs/broad/65536/262144:
                colors: 21
                states: 262,165
                compiled_words: 5,996,826
                forced_words: 39,815,266
                compile_alloc_words: 22,876,976
                compile_major_words: 8,771,131
                compile_promoted_words: 8,771,131
                construct_alloc_words: 11,666,969
                construct_major_words: 2,654,611
                construct_promoted_words: 2,654,611
                forcing: full
              expression IDs/narrow/1024:
                colors: 2
                states: 1,027
                compiled_words: 10,496
                forced_words: 74,513
                compile_alloc_words: 11,062
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/narrow/1000000:
                colors: 2
                states: 1,000,003
                compiled_words: 10,000,256
                forced_words: 72,024,561
                compile_alloc_words: 10,000,822
                compile_major_words: 9,829,801
                compile_promoted_words: 9,829,801
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/tiny:
                colors: 2
                states: 4
                compiled_words: 267
                forced_words: 506
                compile_alloc_words: 874
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/class:
                colors: 2
                states: 5
                compiled_words: 307
                forced_words: 642
                compile_alloc_words: 921
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/pair:
                colors: 3
                states: 7
                compiled_words: 363
                forced_words: 925
                compile_alloc_words: 1,000
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/rotation:
                colors: 2
                states: 5
                compiled_words: 297
                forced_words: 616
                compile_alloc_words: 830
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/short:
                colors: 4
                states: 8
                compiled_words: 385
                forced_words: 1,083
                compile_alloc_words: 1,301
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/categories:
                colors: 2
                states: 5
                compiled_words: 297
                forced_words: 616
                compile_alloc_words: 871
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/boundaries:
                colors: 4
                states: 14
                compiled_words: 415
                forced_words: 1,603
                compile_alloc_words: 1,483
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/literal256:
                colors: 256
                states: 259
                compiled_words: 4,888
                forced_words: 91,081
                compile_alloc_words: 29,071
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/wide run:
                colors: 256
                states: 259
                compiled_words: 4,937
                forced_words: 83,445
                compile_alloc_words: 29,999
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/class run:
                colors: 2
                states: 5
                compiled_words: 307
                forced_words: 606
                compile_alloc_words: 923
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/search:
                colors: 5
                states: 8
                compiled_words: 355
                forced_words: 971
                compile_alloc_words: 1,360
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/alternating:
                colors: 3
                states: 9
                compiled_words: 423
                forced_words: 1,064
                compile_alloc_words: 1,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlapping alternatives:
                colors: 11
                states: 10
                compiled_words: 5,002
                forced_words: 15,249
                compile_alloc_words: 16,247
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/wide alphabet:
                colors: 256
                states: 7
                compiled_words: 7,009
                forced_words: 9,414
                compile_alloc_words: 46,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/token classifier:
                colors: 17
                states: 17
                compiled_words: 1,109
                forced_words: 2,632
                compile_alloc_words: 17,856
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/nested captures:
                colors: 2
                states: 9
                compiled_words: 363
                forced_words: 1,281
                compile_alloc_words: 1,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/repeated bytes4:
                colors: 256
                states: 1,027
                compiled_words: 12,569
                forced_words: 385,249
                compile_alloc_words: 40,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              expression IDs/broad/256/4096:
                colors: 13
                states: 4,109
                compiled_words: 51,353
                forced_words: 407,037
                compile_alloc_words: 97,618
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 20,094
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/captures/sequential/16:
                colors: 2
                states: 19
                compiled_words: 830
                forced_words: 2,312
                compile_alloc_words: 1,741
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/16:
                colors: 2
                states: 35
                compiled_words: 837
                forced_words: 5,254
                compile_alloc_words: 1,772
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/16:
                colors: 2
                states: 35
                compiled_words: 850
                forced_words: 3,824
                compile_alloc_words: 1,790
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/128:
                colors: 2
                states: 131
                compiled_words: 4,862
                forced_words: 15,770
                compile_alloc_words: 8,573
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/128:
                colors: 2
                states: 259
                compiled_words: 4,869
                forced_words: 125,352
                compile_alloc_words: 8,604
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/128:
                colors: 2
                states: 259
                compiled_words: 4,882
                forced_words: 27,826
                compile_alloc_words: 8,622
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/512:
                colors: 2
                states: 515
                compiled_words: 18,686
                forced_words: 62,294
                compile_alloc_words: 31,997
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/512:
                colors: 2
                states: 1,027
                compiled_words: 18,693
                forced_words: 1,680,228
                compile_alloc_words: 32,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/512:
                colors: 2
                states: 1,027
                compiled_words: 18,706
                forced_words: 110,446
                compile_alloc_words: 32,046
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/128:
                colors: 255
                states: 131
                compiled_words: 2,583
                forced_words: 45,830
                compile_alloc_words: 82,809
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/256:
                colors: 255
                states: 259
                compiled_words: 4,119
                forced_words: 90,054
                compile_alloc_words: 94,951
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/512:
                colors: 255
                states: 515
                compiled_words: 7,191
                forced_words: 178,502
                compile_alloc_words: 121,482
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/2048:
                colors: 255
                states: 2,051
                compiled_words: 25,623
                forced_words: 709,190
                compile_alloc_words: 284,880
                compile_major_words: 18,847
                compile_promoted_words: 18,847
                forcing: full
              automata/pathological/100:
                colors: 3
                states: 103
                compiled_words: 1,294
                forced_words: 26,510
                compile_alloc_words: 2,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/pathological/1000:
                colors: 3
                states: 1,003
                compiled_words: 10,294
                forced_words: 2,061,344
                compile_alloc_words: 11,113
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/occupancy/8:
                colors: 256
                states: 772
                compiled_words: 13,092
                forced_words: 272,825
                compile_alloc_words: 60,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/32:
                colors: 256
                states: 772
                compiled_words: 13,092
                forced_words: 272,825
                compile_alloc_words: 60,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/64:
                colors: 256
                states: 772
                compiled_words: 13,092
                forced_words: 272,825
                compile_alloc_words: 60,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/128:
                colors: 256
                states: 772
                compiled_words: 13,092
                forced_words: 272,825
                compile_alloc_words: 60,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/256:
                colors: 256
                states: 772
                compiled_words: 13,092
                forced_words: 272,825
                compile_alloc_words: 60,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/1:
                colors: 256
                states: 2
                compiled_words: 4,903
                forced_words: 5,307
                compile_alloc_words: 30,009
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/8:
                colors: 256
                states: 2
                compiled_words: 4,903
                forced_words: 5,307
                compile_alloc_words: 30,009
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/32:
                colors: 256
                states: 2
                compiled_words: 4,903
                forced_words: 5,307
                compile_alloc_words: 30,009
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/128:
                colors: 256
                states: 2
                compiled_words: 4,903
                forced_words: 5,307
                compile_alloc_words: 30,009
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/1:
                colors: 256
                states: 2
                compiled_words: 4,917
                forced_words: 5,345
                compile_alloc_words: 30,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/8:
                colors: 256
                states: 2
                compiled_words: 4,917
                forced_words: 5,345
                compile_alloc_words: 30,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/32:
                colors: 256
                states: 2
                compiled_words: 4,917
                forced_words: 5,345
                compile_alloc_words: 30,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/128:
                colors: 256
                states: 2
                compiled_words: 4,917
                forced_words: 5,345
                compile_alloc_words: 30,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/1:
                colors: 256
                states: 2
                compiled_words: 4,989
                forced_words: 5,489
                compile_alloc_words: 30,235
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/8:
                colors: 256
                states: 2
                compiled_words: 4,989
                forced_words: 5,489
                compile_alloc_words: 30,235
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/32:
                colors: 256
                states: 2
                compiled_words: 4,989
                forced_words: 5,489
                compile_alloc_words: 30,235
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/128:
                colors: 256
                states: 2
                compiled_words: 4,989
                forced_words: 5,489
                compile_alloc_words: 30,235
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/1:
                colors: 256
                states: 2
                compiled_words: 5,277
                forced_words: 6,065
                compile_alloc_words: 30,919
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/8:
                colors: 256
                states: 2
                compiled_words: 5,277
                forced_words: 6,065
                compile_alloc_words: 30,919
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/32:
                colors: 256
                states: 2
                compiled_words: 5,277
                forced_words: 6,065
                compile_alloc_words: 30,919
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/128:
                colors: 256
                states: 2
                compiled_words: 5,277
                forced_words: 6,065
                compile_alloc_words: 30,919
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/1:
                colors: 256
                states: 2
                compiled_words: 6,429
                forced_words: 8,369
                compile_alloc_words: 33,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/8:
                colors: 256
                states: 2
                compiled_words: 6,429
                forced_words: 8,369
                compile_alloc_words: 33,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/32:
                colors: 256
                states: 2
                compiled_words: 6,429
                forced_words: 8,369
                compile_alloc_words: 33,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/128:
                colors: 256
                states: 2
                compiled_words: 6,429
                forced_words: 8,369
                compile_alloc_words: 33,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/1:
                colors: 23
                states: 189
                compiled_words: 6,832
                forced_words: 38,349
                compile_alloc_words: 13,524
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/8:
                colors: 23
                states: 189
                compiled_words: 6,832
                forced_words: 38,349
                compile_alloc_words: 13,524
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/1:
                colors: 26
                states: 332
                compiled_words: 52,601
                forced_words: 262,761
                compile_alloc_words: 98,956
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/8:
                colors: 26
                states: 332
                compiled_words: 52,601
                forced_words: 262,761
                compile_alloc_words: 98,956
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/1024/1:
                colors: 29
                states: 745
                compiled_words: 422,777
                forced_words: 2,403,099
                compile_alloc_words: 814,840
                compile_major_words: 489,829
                compile_promoted_words: 489,829
                forcing: full
              automata/descriptor width/1024/8:
                colors: 29
                states: 745
                compiled_words: 422,777
                forced_words: 2,403,099
                compile_alloc_words: 814,840
                compile_major_words: 489,829
                compile_promoted_words: 489,829
                forcing: full
              automata/descriptor width/4096/1:
                colors: 31
                states: 1,883
                compiled_words: 1,702,265
                forced_words: 10,786,979
                compile_alloc_words: 3,365,326
                compile_major_words: 2,001,357
                compile_promoted_words: 2,001,357
                forcing: full
              automata/descriptor width/4096/8:
                colors: 31
                states: 1,883
                compiled_words: 1,702,265
                forced_words: 10,786,979
                compile_alloc_words: 3,365,326
                compile_major_words: 2,001,357
                compile_promoted_words: 2,001,357
                forcing: full
              automata/interleaved/1:
                colors: 5
                states: 8
                compiled_words: 440
                forced_words: 986
                compile_alloc_words: 1,396
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/4:
                colors: 11
                states: 11
                compiled_words: 837
                forced_words: 1,976
                compile_alloc_words: 2,864
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/8:
                colors: 12
                states: 16
                compiled_words: 1,323
                forced_words: 3,573
                compile_alloc_words: 4,418
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/16:
                colors: 12
                states: 26
                compiled_words: 2,283
                forced_words: 7,672
                compile_alloc_words: 7,402
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/32:
                colors: 12
                states: 45
                compiled_words: 4,203
                forced_words: 19,661
                compile_alloc_words: 13,370
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              parser/empty:
                colors: 1
                states: 2
                compiled_words: 249
                forced_words: 368
                compile_alloc_words: 833
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/one byte:
                colors: 2
                states: 4
                compiled_words: 267
                forced_words: 506
                compile_alloc_words: 874
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/short literal:
                colors: 13
                states: 37
                compiled_words: 520
                forced_words: 3,971
                compile_alloc_words: 2,010
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 311
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/small alternation:
                colors: 2
                states: 4
                compiled_words: 267
                forced_words: 506
                compile_alloc_words: 942
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 256
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/captures:
                colors: 2
                states: 131
                compiled_words: 4,863
                forced_words: 252,750
                compile_alloc_words: 8,678
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,355
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/noncapturing:
                colors: 3
                states: 259
                compiled_words: 3,333
                forced_words: 209,419
                compile_alloc_words: 12,303
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,475
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/empty groups:
                colors: 1
                states: 2
                compiled_words: 3,321
                forced_words: 6,000
                compile_alloc_words: 8,129
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,323
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/classes:
                colors: 2
                states: 131
                compiled_words: 1,791
                forced_words: 84,646
                compile_alloc_words: 10,276
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,235
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/ranges:
                colors: 2
                states: 131
                compiled_words: 1,791
                forced_words: 84,646
                compile_alloc_words: 13,888
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 11,091
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/escapes:
                colors: 4
                states: 8
                compiled_words: 14,578
                forced_words: 15,319
                compile_alloc_words: 47,279
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,971
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/numeric escapes:
                colors: 5
                states: 515
                compiled_words: 6,417
                forced_words: 335,808
                compile_alloc_words: 16,889
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 9,039
                construct_major_words: 686
                construct_promoted_words: 10
                forcing: full
              parser/named groups:
                colors: 2
                states: 131
                compiled_words: 6,015
                forced_words: 253,902
                compile_alloc_words: 9,830
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,919
                construct_major_words: 526
                construct_promoted_words: 10
                forcing: full
              parser/long name:
                colors: 2
                states: 4
                compiled_words: 811
                forced_words: 1,075
                compile_alloc_words: 913
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 228
                construct_major_words: 1,552
                construct_promoted_words: 10
                forcing: full
              parser/comment 4K:
                colors: 1
                states: 2
                compiled_words: 249
                forced_words: 368
                compile_alloc_words: 833
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              parser/comment 64K:
                colors: 1
                states: 2
                compiled_words: 249
                forced_words: 368
                compile_alloc_words: 833
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 16,398
                construct_promoted_words: 10
                forcing: full
              parser/quoted backslashes:
                colors: 5
                states: 3,075
                compiled_words: 37,137
                forced_words: 7,346,020
                compile_alloc_words: 96,247
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,639
                construct_major_words: 782
                construct_promoted_words: 10
                forcing: full
              parser/quantifiers:
                colors: 5
                states: 1
                compiled_words: 18,705
                forced_words: 18,762
                compile_alloc_words: 51,705
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 7,859
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              nested groups/capturing/64:
                colors: 2
                states: 67
                compiled_words: 1,023
                forced_words: 24,101
                compile_alloc_words: 3,059
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/64:
                colors: 2
                states: 67
                compiled_words: 1,023
                forced_words: 24,101
                compile_alloc_words: 2,867
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/128:
                colors: 2
                states: 131
                compiled_words: 1,791
                forced_words: 84,646
                compile_alloc_words: 5,235
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/128:
                colors: 2
                states: 131
                compiled_words: 1,791
                forced_words: 84,646
                compile_alloc_words: 4,851
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/256:
                colors: 2
                states: 259
                compiled_words: 3,327
                forced_words: 316,456
                compile_alloc_words: 9,587
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/256:
                colors: 2
                states: 259
                compiled_words: 3,327
                forced_words: 316,456
                compile_alloc_words: 8,819
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/512:
                colors: 2
                states: 515
                compiled_words: 6,399
                forced_words: 1,222,444
                compile_alloc_words: 18,291
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/512:
                colors: 2
                states: 515
                compiled_words: 6,399
                forced_words: 1,222,444
                compile_alloc_words: 16,755
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/1024:
                colors: 2
                states: 1,027
                compiled_words: 12,543
                forced_words: 4,803,892
                compile_alloc_words: 35,699
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/1024:
                colors: 2
                states: 1,027
                compiled_words: 12,543
                forced_words: 4,803,892
                compile_alloc_words: 32,627
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              literal/plain/32:
                colors: 26
                states: 35
                compiled_words: 786
                forced_words: 3,872
                compile_alloc_words: 3,206
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/32:
                colors: 26
                states: 35
                compiled_words: 786
                forced_words: 3,872
                compile_alloc_words: 3,206
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/32:
                colors: 27
                states: 67
                compiled_words: 1,144
                forced_words: 7,191
                compile_alloc_words: 3,727
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 479
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/32:
                colors: 28
                states: 36
                compiled_words: 826
                forced_words: 4,080
                compile_alloc_words: 5,459
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 640
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/256:
                colors: 64
                states: 515
                compiled_words: 3,707
                forced_words: 69,715
                compile_alloc_words: 12,629
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/256:
                colors: 64
                states: 515
                compiled_words: 3,707
                forced_words: 69,715
                compile_alloc_words: 12,629
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/256:
                colors: 64
                states: 515
                compiled_words: 6,305
                forced_words: 72,746
                compile_alloc_words: 15,196
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,879
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/256:
                colors: 64
                states: 517
                compiled_words: 3,735
                forced_words: 70,062
                compile_alloc_words: 29,096
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,440
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,787
                forced_words: 1,102,699
                compile_alloc_words: 131,669
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/quoted/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,787
                forced_words: 1,102,699
                compile_alloc_words: 131,669
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/mixed/4096:
                colors: 64
                states: 8,195
                compiled_words: 90,785
                forced_words: 1,151,786
                compile_alloc_words: 172,636
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,849
                construct_major_words: 1,040
                construct_promoted_words: 10
                forcing: full
              literal/alternation/4096:
                colors: 64
                states: 8,197
                compiled_words: 49,815
                forced_words: 1,104,006
                compile_alloc_words: 393,896
                compile_major_words: 164,172
                compile_promoted_words: 164,172
                construct_alloc_words: 49,388
                construct_major_words: 2,062
                construct_promoted_words: 10
                forcing: full
              literal/plain/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,067
                forced_words: 17,635,177
                compile_alloc_words: 2,036,309
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/quoted/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,067
                forced_words: 17,635,177
                compile_alloc_words: 2,036,309
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/mixed/65536:
                colors: 64
                states: 131,075
                compiled_words: 1,442,465
                forced_words: 18,416,426
                compile_alloc_words: 2,691,676
                compile_major_words: 1,568,909
                compile_promoted_words: 1,568,909
                construct_alloc_words: 393,489
                construct_major_words: 344,273
                construct_promoted_words: 327,883
                forcing: full
              literal/hit/32:
                colors: 26
                states: 35
                compiled_words: 786
                forced_words: 3,872
                compile_alloc_words: 3,206
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/32:
                colors: 26
                states: 35
                compiled_words: 786
                forced_words: 3,872
                compile_alloc_words: 3,206
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/32:
                colors: 26
                states: 35
                compiled_words: 786
                forced_words: 3,872
                compile_alloc_words: 3,206
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,551
                compile_alloc_words: 3,002
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 425
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/256:
                colors: 64
                states: 515
                compiled_words: 3,707
                forced_words: 69,715
                compile_alloc_words: 12,629
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/256:
                colors: 64
                states: 515
                compiled_words: 3,707
                forced_words: 69,715
                compile_alloc_words: 12,629
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/256:
                colors: 64
                states: 515
                compiled_words: 3,707
                forced_words: 69,715
                compile_alloc_words: 12,629
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/256:
                colors: 64
                states: 259
                compiled_words: 3,706
                forced_words: 34,380
                compile_alloc_words: 12,311
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,825
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,787
                forced_words: 1,102,699
                compile_alloc_words: 131,669
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/absent/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,787
                forced_words: 1,102,699
                compile_alloc_words: 131,669
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/late miss/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,787
                forced_words: 1,102,699
                compile_alloc_words: 131,669
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/anchored/4096:
                colors: 64
                states: 4,099
                compiled_words: 49,786
                forced_words: 538,380
                compile_alloc_words: 131,351
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,797
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/overlap/256:
                colors: 3
                states: 1,021
                compiled_words: 3,333
                forced_words: 943,576
                compile_alloc_words: 8,860
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/capturing:
                colors: 48
                states: 6,947
                compiled_words: 383,325
                forced_words: 1,278,368
                compile_alloc_words: 1,034,748
                compile_major_words: 558,178
                compile_promoted_words: 558,178
                construct_alloc_words: 220,816
                construct_major_words: 9,880
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/noncapturing:
                colors: 48
                states: 6,947
                compiled_words: 106,037
                forced_words: 893,130
                compile_alloc_words: 1,369,624
                compile_major_words: 470,378
                compile_promoted_words: 470,378
                construct_alloc_words: 216,586
                construct_major_words: 10,586
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/capturing:
                colors: 42
                states: 375
                compiled_words: 17,987
                forced_words: 501,969
                compile_alloc_words: 64,377
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,040
                construct_major_words: 834
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/noncapturing:
                colors: 42
                states: 375
                compiled_words: 17,987
                forced_words: 501,969
                compile_alloc_words: 63,861
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 16,879
                construct_major_words: 1,028
                construct_promoted_words: 10
                forcing: full
              boolean/literal-short-hit:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-miss:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-late:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-overlap-miss:
                colors: 3
                states: 65
                compiled_words: 465
                forced_words: 7,862
                compile_alloc_words: 1,453
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/alt-long-miss:
                colors: 10
                states: 15
                compiled_words: 472
                forced_words: 1,563
                compile_alloc_words: 1,812
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/suffix-miss:
                colors: 4
                states: 5
                compiled_words: 315
                forced_words: 708
                compile_alloc_words: 1,123
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/star-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 274
                forced_words: 599
                compile_alloc_words: 918
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/plus-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 284
                forced_words: 602
                compile_alloc_words: 928
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-repeat:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 562
                compile_alloc_words: 823
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 562
                compile_alloc_words: 850
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-varied:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 562
                compile_alloc_words: 850
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-fail:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 562
                compile_alloc_words: 850
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-hit:
                colors: 6
                states: 10
                compiled_words: 411
                forced_words: 1,124
                compile_alloc_words: 1,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-miss:
                colors: 6
                states: 10
                compiled_words: 411
                forced_words: 1,124
                compile_alloc_words: 1,655
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/http-hit:
                colors: 18
                states: 30
                compiled_words: 711
                forced_words: 3,418
                compile_alloc_words: 2,945
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-miss:
                colors: 6
                states: 12
                compiled_words: 393
                forced_words: 1,298
                compile_alloc_words: 1,445
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/line-anchor-miss:
                colors: 5
                states: 11
                compiled_words: 351
                forced_words: 1,162
                compile_alloc_words: 1,272
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/longest-hit:
                colors: 3
                states: 9
                compiled_words: 328
                forced_words: 952
                compile_alloc_words: 1,122
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/ambiguous-miss:
                colors: 3
                states: 7
                compiled_words: 320
                forced_words: 824
                compile_alloc_words: 1,157
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-reject:
                colors: 7
                states: 9
                compiled_words: 356
                forced_words: 910
                compile_alloc_words: 1,220
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-accept:
                colors: 7
                states: 9
                compiled_words: 356
                forced_words: 910
                compile_alloc_words: 1,220
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-candidates:
                colors: 3
                states: 259
                compiled_words: 3,333
                forced_words: 20,661
                compile_alloc_words: 8,860
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-short:
                colors: 3
                states: 11
                compiled_words: 357
                forced_words: 1,065
                compile_alloc_words: 1,172
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/cycle-anchored:
                colors: 3
                states: 6
                compiled_words: 311
                forced_words: 661
                compile_alloc_words: 929
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/class-late-hit:
                colors: 3
                states: 6
                compiled_words: 302
                forced_words: 738
                compile_alloc_words: 1,062
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-late-hit:
                colors: 6
                states: 12
                compiled_words: 393
                forced_words: 1,298
                compile_alloc_words: 1,445
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1k-miss:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1m-miss:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/path-glob:
                colors: 6
                states: 16
                compiled_words: 734
                forced_words: 2,757
                compile_alloc_words: 3,637
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/captures:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,195
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/no group:
                colors: 5
                states: 9
                compiled_words: 345
                forced_words: 936
                compile_alloc_words: 1,208
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/captures:
                colors: 3
                states: 125
                compiled_words: 645
                forced_words: 21,204
                compile_alloc_words: 1,903
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/no group:
                colors: 3
                states: 125
                compiled_words: 645
                forced_words: 21,204
                compile_alloc_words: 1,916
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/captures:
                colors: 6
                states: 11
                compiled_words: 483
                forced_words: 1,365
                compile_alloc_words: 1,732
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/no group:
                colors: 6
                states: 10
                compiled_words: 411
                forced_words: 1,124
                compile_alloc_words: 1,664
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/captures:
                colors: 18
                states: 30
                compiled_words: 783
                forced_words: 3,689
                compile_alloc_words: 3,076
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/no group:
                colors: 18
                states: 29
                compiled_words: 711
                forced_words: 3,338
                compile_alloc_words: 3,008
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/captures:
                colors: 3
                states: 10
                compiled_words: 392
                forced_words: 1,289
                compile_alloc_words: 1,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/no group:
                colors: 3
                states: 7
                compiled_words: 320
                forced_words: 824
                compile_alloc_words: 1,166
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/captures:
                colors: 6
                states: 12
                compiled_words: 417
                forced_words: 1,370
                compile_alloc_words: 1,462
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/no group:
                colors: 6
                states: 12
                compiled_words: 393
                forced_words: 1,298
                compile_alloc_words: 1,448
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/captures:
                colors: 2
                states: 35
                compiled_words: 1,854
                forced_words: 84,176
                compile_alloc_words: 4,317
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/no group:
                colors: 2
                states: 35
                compiled_words: 1,086
                forced_words: 5,689
                compile_alloc_words: 3,466
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/captures:
                colors: 2
                states: 35
                compiled_words: 1,470
                forced_words: 28,357
                compile_alloc_words: 3,933
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/no group:
                colors: 2
                states: 35
                compiled_words: 1,470
                forced_words: 28,357
                compile_alloc_words: 3,946
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/literal:
                colors: 9
                states: 11
                compiled_words: 394
                forced_words: 1,162
                compile_alloc_words: 1,508
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/greedy:
                colors: 3
                states: 25
                compiled_words: 328
                forced_words: 2,638
                compile_alloc_words: 1,081
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/lazy:
                colors: 3
                states: 11
                compiled_words: 328
                forced_words: 1,202
                compile_alloc_words: 1,095
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/captures:
                colors: 4
                states: 9
                compiled_words: 386
                forced_words: 1,057
                compile_alloc_words: 1,151
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/word boundaries:
                colors: 2
                states: 8
                compiled_words: 356
                forced_words: 948
                compile_alloc_words: 1,174
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/broad alternatives:
                colors: 12
                states: 34
                compiled_words: 2,224
                forced_words: 7,852
                compile_alloc_words: 7,496
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/fixed repetition:
                colors: 2
                states: 131
                compiled_words: 1,536
                forced_words: 9,553
                compile_alloc_words: 2,102
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/longest:
                colors: 2
                states: 21
                compiled_words: 320
                forced_words: 2,396
                compile_alloc_words: 1,036
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nullable:
                colors: 2
                states: 5
                compiled_words: 312
                forced_words: 733
                compile_alloc_words: 985
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nested erasure:
                colors: 3
                states: 9
                compiled_words: 494
                forced_words: 1,238
                compile_alloc_words: 1,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/capture loop:
                colors: 4
                states: 13
                compiled_words: 400
                forced_words: 1,489
                compile_alloc_words: 1,092
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/unanchored bytes:
                colors: 256
                states: 259
                compiled_words: 4,889
                forced_words: 92,147
                compile_alloc_words: 29,938
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/sequential32:
                colors: 2
                states: 35
                compiled_words: 1,406
                forced_words: 4,235
                compile_alloc_words: 2,717
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion email:
                colors: 6
                states: 35
                compiled_words: 412
                forced_words: 3,538
                compile_alloc_words: 1,716
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion http:
                colors: 16
                states: 27
                compiled_words: 717
                forced_words: 3,183
                compile_alloc_words: 2,780
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/tex:
                colors: 42
                states: 68
                compiled_words: 11,363
                forced_words: 81,986
                compile_alloc_words: 51,774
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              words/dictionary/250:
                colors: 27
                states: 8,135
                compiled_words: 23,756
                forced_words: 992,145
                compile_alloc_words: 89,603
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 8,242
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/500:
                colors: 27
                states: 19,768
                compiled_words: 45,308
                forced_words: 2,442,247
                compile_alloc_words: 180,262
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 16,542
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/1000/whole-string:
                colors: 27
                states: 6,034
                compiled_words: 83,439
                forced_words: 608,528
                compile_alloc_words: 353,468
                compile_major_words: 139,804
                compile_promoted_words: 139,804
                construct_alloc_words: 32,403
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/2500/whole-string:
                colors: 27
                states: 14,174
                compiled_words: 197,703
                forced_words: 1,430,169
                compile_alloc_words: 901,185
                compile_major_words: 349,197
                compile_promoted_words: 349,197
                construct_alloc_words: 81,630
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/5000/whole-string:
                colors: 27
                states: 26,331
                compiled_words: 371,919
                forced_words: 2,663,249
                compile_alloc_words: 1,818,892
                compile_major_words: 695,989
                compile_promoted_words: 695,989
                construct_alloc_words: 162,656
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/5000/word-boundary:
                colors: 28
                states: 26,334
                compiled_words: 371,968
                forced_words: 3,163,952
                compile_alloc_words: 1,819,299
                compile_major_words: 696,095
                compile_promoted_words: 696,095
                construct_alloc_words: 162,656
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              |}]
          ;;
        end
        in
        ()
    ;;
  end)
;;
