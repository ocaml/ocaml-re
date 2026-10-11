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
                compiled_words: 491
                forced_words: 3,856
                compile_alloc_words: 1,486
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              lots of a's:
                colors: 3
                states: 9
                compiled_words: 307
                forced_words: 901
                compile_alloc_words: 1,050
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              media type match:
                colors: 3
                states: 10
                compiled_words: 316
                forced_words: 1,084
                compile_alloc_words: 1,135
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 36,756
                compile_alloc_words: 2,854
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              tex gitignore:
                colors: 42
                states: 68
                compiled_words: 11,359
                forced_words: 81,982
                compile_alloc_words: 51,757
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/no group:
                colors: 13
                states: 557
                compiled_words: 958
                forced_words: 43,704
                compile_alloc_words: 4,000
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/group:
                colors: 13
                states: 374
                compiled_words: 838
                forced_words: 58,100
                compile_alloc_words: 3,878
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              http/auto/execp no group:
                colors: 13
                states: 799
                compiled_words: 1,497
                forced_words: 97,762
                compile_alloc_words: 4,568
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/auto/all_gen:
                colors: 13
                states: 707
                compiled_words: 1,277
                forced_words: 121,883
                compile_alloc_words: 4,346
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              string traversal from #210:
                colors: 3
                states: 65
                compiled_words: 461
                forced_words: 7,858
                compile_alloc_words: 1,436
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              kleene star compilation:
                colors: 2
                states: 5
                compiled_words: 270
                forced_words: 595
                compile_alloc_words: 901
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1:
                colors: 3
                states: 1,003
                compiled_words: 10,290
                forced_words: 2,061,340
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2:
                colors: 4
                states: 1,003
                compiled_words: 10,308
                forced_words: 4,564,597
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              repeated sequence re:
                colors: 256
                states: 12,803
                compiled_words: 130,325
                forced_words: 8,906,439
                compile_alloc_words: 157,960
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              split on whitespace:
                colors: 2
                states: 5
                compiled_words: 280
                forced_words: 646
                compile_alloc_words: 951
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              shared prefixes:
                colors: 27
                states: 8
                compiled_words: 6,204
                forced_words: 11,296
                compile_alloc_words: 24,993
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/1:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 36,756
                compile_alloc_words: 2,854
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/2:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 36,756
                compile_alloc_words: 2,854
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/3:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 36,756
                compile_alloc_words: 2,854
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/email-html5:
                colors: 7
                states: 256
                compiled_words: 3,348
                forced_words: 28,676
                compile_alloc_words: 8,896
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/url:
                colors: 12
                states: 17
                compiled_words: 516
                forced_words: 2,052
                compile_alloc_words: 2,321
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv4:
                colors: 8
                states: 35
                compiled_words: 966
                forced_words: 5,002
                compile_alloc_words: 3,240
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv6:
                colors: 16
                states: 1,244
                compiled_words: 11,200
                forced_words: 258,969
                compile_alloc_words: 23,240
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/mac:
                colors: 4
                states: 21
                compiled_words: 560
                forced_words: 2,425
                compile_alloc_words: 1,528
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-date:
                colors: 8
                states: 17
                compiled_words: 536
                forced_words: 1,969
                compile_alloc_words: 2,061
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/time-24h:
                colors: 8
                states: 13
                compiled_words: 418
                forced_words: 1,423
                compile_alloc_words: 1,755
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-timestamp:
                colors: 9
                states: 31
                compiled_words: 666
                forced_words: 3,212
                compile_alloc_words: 2,811
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/phone-us:
                colors: 8
                states: 35
                compiled_words: 576
                forced_words: 3,563
                compile_alloc_words: 2,305
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/credit-card:
                colors: 11
                states: 66
                compiled_words: 1,011
                forced_words: 6,273
                compile_alloc_words: 2,968
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/uuid:
                colors: 6
                states: 40
                compiled_words: 650
                forced_words: 3,829
                compile_alloc_words: 2,096
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/semver:
                colors: 8
                states: 41
                compiled_words: 923
                forced_words: 5,226
                compile_alloc_words: 4,277
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/base64:
                colors: 4
                states: 12
                compiled_words: 413
                forced_words: 1,393
                compile_alloc_words: 1,738
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/domain:
                colors: 6
                states: 316
                compiled_words: 4,791
                forced_words: 37,848
                compile_alloc_words: 8,062
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/apache-combined:
                colors: 8
                states: 82
                compiled_words: 960
                forced_words: 8,599
                compile_alloc_words: 3,308
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/syslog-rfc3164:
                colors: 13
                states: 33
                compiled_words: 782
                forced_words: 3,315
                compile_alloc_words: 3,107
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/structured-line:
                colors: 13
                states: 37
                compiled_words: 846
                forced_words: 3,288
                compile_alloc_words: 3,289
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/csv/rfc4180-row:
                colors: 5
                states: 16
                compiled_words: 481
                forced_words: 1,957
                compile_alloc_words: 2,100
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/string:
                colors: 8
                states: 12
                compiled_words: 435
                forced_words: 1,467
                compile_alloc_words: 1,862
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/number:
                colors: 8
                states: 14
                compiled_words: 495
                forced_words: 1,655
                compile_alloc_words: 2,073
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/tag:
                colors: 7
                states: 26
                compiled_words: 495
                forced_words: 2,803
                compile_alloc_words: 1,995
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/comment:
                colors: 5
                states: 15
                compiled_words: 372
                forced_words: 1,626
                compile_alloc_words: 1,349
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/markdown/link:
                colors: 7
                states: 17
                compiled_words: 571
                forced_words: 1,526
                compile_alloc_words: 2,331
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/comment/c-style:
                colors: 3
                states: 27
                compiled_words: 426
                forced_words: 3,001
                compile_alloc_words: 1,659
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/keywords:
                colors: 26
                states: 225
                compiled_words: 3,804
                forced_words: 27,175
                compile_alloc_words: 16,561
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/lexer:
                colors: 19
                states: 246
                compiled_words: 1,119
                forced_words: 35,562
                compile_alloc_words: 6,793
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/number-literals:
                colors: 15
                states: 27
                compiled_words: 787
                forced_words: 3,384
                compile_alloc_words: 4,318
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/number/roman:
                colors: 9
                states: 30
                compiled_words: 817
                forced_words: 3,416
                compile_alloc_words: 2,662
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/money/usd:
                colors: 6
                states: 15
                compiled_words: 439
                forced_words: 1,641
                compile_alloc_words: 1,528
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/docker/reference:
                colors: 18
                states: 344
                compiled_words: 4,458
                forced_words: 43,142
                compile_alloc_words: 12,101
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/aws-access-key:
                colors: 9
                states: 5,473
                compiled_words: 608
                forced_words: 610,947
                compile_alloc_words: 2,283
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/api-key-line:
                colors: 18
                states: 893
                compiled_words: 840
                forced_words: 95,720
                compile_alloc_words: 4,985
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/youtube/video-id:
                colors: 21
                states: 294
                compiled_words: 957
                forced_words: 33,076
                compile_alloc_words: 4,007
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/date/us-slash:
                colors: 8
                states: 95
                compiled_words: 648
                forced_words: 10,706
                compile_alloc_words: 2,834
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/seq-of-reps:
                colors: 10
                states: 83
                compiled_words: 720
                forced_words: 13,396
                compile_alloc_words: 2,123
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/capture-stars:
                colors: 6
                states: 305
                compiled_words: 785
                forced_words: 59,221
                compile_alloc_words: 1,822
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/star-plus:
                colors: 4
                states: 21
                compiled_words: 423
                forced_words: 2,304
                compile_alloc_words: 1,130
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-of-reps:
                colors: 6
                states: 22
                compiled_words: 471
                forced_words: 2,658
                compile_alloc_words: 1,442
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/flat-reps:
                colors: 5
                states: 17
                compiled_words: 613
                forced_words: 2,279
                compile_alloc_words: 1,439
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/word-digit:
                colors: 4
                states: 11
                compiled_words: 419
                forced_words: 1,247
                compile_alloc_words: 1,182
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-literals:
                colors: 9
                states: 13
                compiled_words: 588
                forced_words: 1,766
                compile_alloc_words: 1,852
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/single-rep:
                colors: 4
                states: 13
                compiled_words: 365
                forced_words: 1,395
                compile_alloc_words: 1,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/adjacent:
                colors: 8
                states: 8
                compiled_words: 365
                forced_words: 912
                compile_alloc_words: 1,441
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/interleaved:
                colors: 5
                states: 6
                compiled_words: 364
                forced_words: 756
                compile_alloc_words: 1,210
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/4:
                colors: 11
                states: 11
                compiled_words: 833
                forced_words: 1,972
                compile_alloc_words: 2,860
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/16:
                colors: 12
                states: 26
                compiled_words: 2,279
                forced_words: 7,668
                compile_alloc_words: 7,398
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/log files:
                colors: 10
                states: 30
                compiled_words: 1,115
                forced_words: 4,021
                compile_alloc_words: 3,294
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/escape tokens:
                colors: 10
                states: 16
                compiled_words: 623
                forced_words: 2,021
                compile_alloc_words: 2,398
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/routes:
                colors: 21
                states: 65
                compiled_words: 1,271
                forced_words: 8,477
                compile_alloc_words: 4,418
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1/10:
                colors: 3
                states: 11
                compiled_words: 10,290
                forced_words: 11,224
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/20:
                colors: 3
                states: 21
                compiled_words: 10,290
                forced_words: 12,244
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/40:
                colors: 3
                states: 41
                compiled_words: 10,290
                forced_words: 15,484
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/80:
                colors: 3
                states: 81
                compiled_words: 10,290
                forced_words: 26,764
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/100:
                colors: 3
                states: 101
                compiled_words: 10,290
                forced_words: 34,804
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/1000:
                colors: 3
                states: 1,001
                compiled_words: 10,290
                forced_words: 2,052,988
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/10:
                colors: 4
                states: 11
                compiled_words: 10,308
                forced_words: 11,434
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/20:
                colors: 4
                states: 21
                compiled_words: 10,308
                forced_words: 13,139
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/40:
                colors: 4
                states: 41
                compiled_words: 10,308
                forced_words: 19,249
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/80:
                colors: 4
                states: 81
                compiled_words: 10,308
                forced_words: 42,269
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/100:
                colors: 4
                states: 101
                compiled_words: 10,308
                forced_words: 59,179
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/1000:
                colors: 4
                states: 1,001
                compiled_words: 10,308
                forced_words: 4,546,513
                compile_alloc_words: 11,183
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              expression IDs/broad/16/1024:
                colors: 9
                states: 1,033
                compiled_words: 11,053
                forced_words: 92,585
                compile_alloc_words: 14,067
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 801
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/broad/4096/16384:
                colors: 17
                states: 16,401
                compiled_words: 350,486
                forced_words: 2,293,394
                compile_alloc_words: 1,248,163
                compile_major_words: 407,092
                compile_promoted_words: 407,092
                construct_alloc_words: 500,327
                construct_major_words: 141,345
                construct_promoted_words: 141,345
                forcing: full
              expression IDs/broad/65536/262144:
                colors: 21
                states: 262,165
                compiled_words: 5,996,822
                forced_words: 39,815,262
                compile_alloc_words: 22,876,972
                compile_major_words: 8,771,245
                compile_promoted_words: 8,771,245
                construct_alloc_words: 11,666,969
                construct_major_words: 2,654,611
                construct_promoted_words: 2,654,611
                forcing: full
              expression IDs/narrow/1024:
                colors: 2
                states: 1,027
                compiled_words: 10,492
                forced_words: 74,509
                compile_alloc_words: 11,058
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/narrow/1000000:
                colors: 2
                states: 1,000,003
                compiled_words: 10,000,252
                forced_words: 72,024,557
                compile_alloc_words: 10,000,818
                compile_major_words: 9,829,801
                compile_promoted_words: 9,829,801
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/tiny:
                colors: 2
                states: 4
                compiled_words: 263
                forced_words: 502
                compile_alloc_words: 870
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/class:
                colors: 2
                states: 5
                compiled_words: 303
                forced_words: 638
                compile_alloc_words: 917
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/pair:
                colors: 3
                states: 7
                compiled_words: 359
                forced_words: 921
                compile_alloc_words: 996
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/rotation:
                colors: 2
                states: 5
                compiled_words: 293
                forced_words: 612
                compile_alloc_words: 826
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/short:
                colors: 4
                states: 8
                compiled_words: 381
                forced_words: 1,079
                compile_alloc_words: 1,297
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/categories:
                colors: 2
                states: 5
                compiled_words: 293
                forced_words: 612
                compile_alloc_words: 867
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/boundaries:
                colors: 4
                states: 14
                compiled_words: 411
                forced_words: 1,599
                compile_alloc_words: 1,479
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/literal256:
                colors: 256
                states: 259
                compiled_words: 4,884
                forced_words: 91,077
                compile_alloc_words: 29,067
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/wide run:
                colors: 256
                states: 259
                compiled_words: 4,933
                forced_words: 83,441
                compile_alloc_words: 29,995
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/class run:
                colors: 2
                states: 5
                compiled_words: 303
                forced_words: 602
                compile_alloc_words: 919
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/search:
                colors: 5
                states: 8
                compiled_words: 351
                forced_words: 967
                compile_alloc_words: 1,356
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/alternating:
                colors: 3
                states: 9
                compiled_words: 419
                forced_words: 1,060
                compile_alloc_words: 1,182
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlapping alternatives:
                colors: 11
                states: 10
                compiled_words: 4,998
                forced_words: 15,245
                compile_alloc_words: 16,243
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/wide alphabet:
                colors: 256
                states: 7
                compiled_words: 7,005
                forced_words: 9,410
                compile_alloc_words: 46,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/token classifier:
                colors: 17
                states: 17
                compiled_words: 1,105
                forced_words: 2,628
                compile_alloc_words: 17,852
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/nested captures:
                colors: 2
                states: 9
                compiled_words: 359
                forced_words: 1,277
                compile_alloc_words: 1,105
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/repeated bytes4:
                colors: 256
                states: 1,027
                compiled_words: 12,565
                forced_words: 385,245
                compile_alloc_words: 40,200
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              expression IDs/broad/256/4096:
                colors: 13
                states: 4,109
                compiled_words: 51,349
                forced_words: 407,033
                compile_alloc_words: 97,614
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 20,094
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/captures/sequential/16:
                colors: 2
                states: 19
                compiled_words: 826
                forced_words: 2,308
                compile_alloc_words: 1,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/16:
                colors: 2
                states: 35
                compiled_words: 833
                forced_words: 5,250
                compile_alloc_words: 1,768
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/16:
                colors: 2
                states: 35
                compiled_words: 846
                forced_words: 3,820
                compile_alloc_words: 1,786
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/128:
                colors: 2
                states: 131
                compiled_words: 4,858
                forced_words: 15,766
                compile_alloc_words: 8,569
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/128:
                colors: 2
                states: 259
                compiled_words: 4,865
                forced_words: 125,348
                compile_alloc_words: 8,600
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/128:
                colors: 2
                states: 259
                compiled_words: 4,878
                forced_words: 27,822
                compile_alloc_words: 8,618
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/512:
                colors: 2
                states: 515
                compiled_words: 18,682
                forced_words: 62,290
                compile_alloc_words: 31,993
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/512:
                colors: 2
                states: 1,027
                compiled_words: 18,689
                forced_words: 1,680,224
                compile_alloc_words: 32,024
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/512:
                colors: 2
                states: 1,027
                compiled_words: 18,702
                forced_words: 110,442
                compile_alloc_words: 32,042
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/128:
                colors: 255
                states: 131
                compiled_words: 2,579
                forced_words: 45,826
                compile_alloc_words: 82,805
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/256:
                colors: 255
                states: 259
                compiled_words: 4,115
                forced_words: 90,050
                compile_alloc_words: 94,947
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/512:
                colors: 255
                states: 515
                compiled_words: 7,187
                forced_words: 178,498
                compile_alloc_words: 121,478
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/2048:
                colors: 255
                states: 2,051
                compiled_words: 25,619
                forced_words: 709,186
                compile_alloc_words: 284,876
                compile_major_words: 18,847
                compile_promoted_words: 18,847
                forcing: full
              automata/pathological/100:
                colors: 3
                states: 103
                compiled_words: 1,290
                forced_words: 26,506
                compile_alloc_words: 2,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/pathological/1000:
                colors: 3
                states: 1,003
                compiled_words: 10,290
                forced_words: 2,061,340
                compile_alloc_words: 11,109
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/occupancy/8:
                colors: 256
                states: 772
                compiled_words: 13,088
                forced_words: 272,821
                compile_alloc_words: 60,734
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/32:
                colors: 256
                states: 772
                compiled_words: 13,088
                forced_words: 272,821
                compile_alloc_words: 60,734
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/64:
                colors: 256
                states: 772
                compiled_words: 13,088
                forced_words: 272,821
                compile_alloc_words: 60,734
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/128:
                colors: 256
                states: 772
                compiled_words: 13,088
                forced_words: 272,821
                compile_alloc_words: 60,734
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/256:
                colors: 256
                states: 772
                compiled_words: 13,088
                forced_words: 272,821
                compile_alloc_words: 60,734
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/1:
                colors: 256
                states: 2
                compiled_words: 4,899
                forced_words: 5,303
                compile_alloc_words: 30,005
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/8:
                colors: 256
                states: 2
                compiled_words: 4,899
                forced_words: 5,303
                compile_alloc_words: 30,005
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/32:
                colors: 256
                states: 2
                compiled_words: 4,899
                forced_words: 5,303
                compile_alloc_words: 30,005
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/128:
                colors: 256
                states: 2
                compiled_words: 4,899
                forced_words: 5,303
                compile_alloc_words: 30,005
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/1:
                colors: 256
                states: 2
                compiled_words: 4,913
                forced_words: 5,341
                compile_alloc_words: 30,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/8:
                colors: 256
                states: 2
                compiled_words: 4,913
                forced_words: 5,341
                compile_alloc_words: 30,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/32:
                colors: 256
                states: 2
                compiled_words: 4,913
                forced_words: 5,341
                compile_alloc_words: 30,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/128:
                colors: 256
                states: 2
                compiled_words: 4,913
                forced_words: 5,341
                compile_alloc_words: 30,028
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/1:
                colors: 256
                states: 2
                compiled_words: 4,985
                forced_words: 5,485
                compile_alloc_words: 30,231
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/8:
                colors: 256
                states: 2
                compiled_words: 4,985
                forced_words: 5,485
                compile_alloc_words: 30,231
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/32:
                colors: 256
                states: 2
                compiled_words: 4,985
                forced_words: 5,485
                compile_alloc_words: 30,231
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/128:
                colors: 256
                states: 2
                compiled_words: 4,985
                forced_words: 5,485
                compile_alloc_words: 30,231
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/1:
                colors: 256
                states: 2
                compiled_words: 5,273
                forced_words: 6,061
                compile_alloc_words: 30,915
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/8:
                colors: 256
                states: 2
                compiled_words: 5,273
                forced_words: 6,061
                compile_alloc_words: 30,915
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/32:
                colors: 256
                states: 2
                compiled_words: 5,273
                forced_words: 6,061
                compile_alloc_words: 30,915
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/128:
                colors: 256
                states: 2
                compiled_words: 5,273
                forced_words: 6,061
                compile_alloc_words: 30,915
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/1:
                colors: 256
                states: 2
                compiled_words: 6,425
                forced_words: 8,365
                compile_alloc_words: 33,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/8:
                colors: 256
                states: 2
                compiled_words: 6,425
                forced_words: 8,365
                compile_alloc_words: 33,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/32:
                colors: 256
                states: 2
                compiled_words: 6,425
                forced_words: 8,365
                compile_alloc_words: 33,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/128:
                colors: 256
                states: 2
                compiled_words: 6,425
                forced_words: 8,365
                compile_alloc_words: 33,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/1:
                colors: 23
                states: 189
                compiled_words: 6,828
                forced_words: 38,345
                compile_alloc_words: 13,520
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/8:
                colors: 23
                states: 189
                compiled_words: 6,828
                forced_words: 38,345
                compile_alloc_words: 13,520
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/1:
                colors: 26
                states: 332
                compiled_words: 52,597
                forced_words: 262,757
                compile_alloc_words: 98,952
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/8:
                colors: 26
                states: 332
                compiled_words: 52,597
                forced_words: 262,757
                compile_alloc_words: 98,952
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/1024/1:
                colors: 29
                states: 745
                compiled_words: 422,773
                forced_words: 2,403,095
                compile_alloc_words: 814,836
                compile_major_words: 490,154
                compile_promoted_words: 490,154
                forcing: full
              automata/descriptor width/1024/8:
                colors: 29
                states: 745
                compiled_words: 422,773
                forced_words: 2,403,095
                compile_alloc_words: 814,836
                compile_major_words: 490,154
                compile_promoted_words: 490,154
                forcing: full
              automata/descriptor width/4096/1:
                colors: 31
                states: 1,883
                compiled_words: 1,702,261
                forced_words: 10,786,975
                compile_alloc_words: 3,365,322
                compile_major_words: 2,070,480
                compile_promoted_words: 2,070,480
                forcing: full
              automata/descriptor width/4096/8:
                colors: 31
                states: 1,883
                compiled_words: 1,702,261
                forced_words: 10,786,975
                compile_alloc_words: 3,365,322
                compile_major_words: 2,070,480
                compile_promoted_words: 2,070,480
                forcing: full
              automata/interleaved/1:
                colors: 5
                states: 8
                compiled_words: 436
                forced_words: 982
                compile_alloc_words: 1,392
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/4:
                colors: 11
                states: 11
                compiled_words: 833
                forced_words: 1,972
                compile_alloc_words: 2,860
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/8:
                colors: 12
                states: 16
                compiled_words: 1,319
                forced_words: 3,569
                compile_alloc_words: 4,414
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/16:
                colors: 12
                states: 26
                compiled_words: 2,279
                forced_words: 7,668
                compile_alloc_words: 7,398
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/32:
                colors: 12
                states: 45
                compiled_words: 4,199
                forced_words: 19,657
                compile_alloc_words: 13,366
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              parser/empty:
                colors: 1
                states: 2
                compiled_words: 245
                forced_words: 364
                compile_alloc_words: 829
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/one byte:
                colors: 2
                states: 4
                compiled_words: 263
                forced_words: 502
                compile_alloc_words: 870
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/short literal:
                colors: 13
                states: 37
                compiled_words: 516
                forced_words: 3,967
                compile_alloc_words: 2,006
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 311
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/small alternation:
                colors: 2
                states: 4
                compiled_words: 263
                forced_words: 502
                compile_alloc_words: 938
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 256
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/captures:
                colors: 2
                states: 131
                compiled_words: 4,859
                forced_words: 252,746
                compile_alloc_words: 8,674
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,355
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/noncapturing:
                colors: 3
                states: 259
                compiled_words: 3,329
                forced_words: 209,415
                compile_alloc_words: 12,299
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,475
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/empty groups:
                colors: 1
                states: 2
                compiled_words: 3,317
                forced_words: 5,996
                compile_alloc_words: 8,125
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,323
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/classes:
                colors: 2
                states: 131
                compiled_words: 1,787
                forced_words: 84,642
                compile_alloc_words: 10,272
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,235
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/ranges:
                colors: 2
                states: 131
                compiled_words: 1,787
                forced_words: 84,642
                compile_alloc_words: 13,884
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 11,091
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/escapes:
                colors: 4
                states: 8
                compiled_words: 14,574
                forced_words: 15,315
                compile_alloc_words: 47,275
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,971
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/numeric escapes:
                colors: 5
                states: 515
                compiled_words: 6,413
                forced_words: 335,804
                compile_alloc_words: 16,885
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 9,039
                construct_major_words: 686
                construct_promoted_words: 10
                forcing: full
              parser/named groups:
                colors: 2
                states: 131
                compiled_words: 6,011
                forced_words: 253,898
                compile_alloc_words: 9,826
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,919
                construct_major_words: 526
                construct_promoted_words: 10
                forcing: full
              parser/long name:
                colors: 2
                states: 4
                compiled_words: 807
                forced_words: 1,071
                compile_alloc_words: 909
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 228
                construct_major_words: 1,552
                construct_promoted_words: 10
                forcing: full
              parser/comment 4K:
                colors: 1
                states: 2
                compiled_words: 245
                forced_words: 364
                compile_alloc_words: 829
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              parser/comment 64K:
                colors: 1
                states: 2
                compiled_words: 245
                forced_words: 364
                compile_alloc_words: 829
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 16,398
                construct_promoted_words: 10
                forcing: full
              parser/quoted backslashes:
                colors: 5
                states: 3,075
                compiled_words: 37,133
                forced_words: 7,346,016
                compile_alloc_words: 96,243
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,639
                construct_major_words: 782
                construct_promoted_words: 10
                forcing: full
              parser/quantifiers:
                colors: 5
                states: 1
                compiled_words: 18,701
                forced_words: 18,758
                compile_alloc_words: 51,701
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 7,859
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              literal/plain/32:
                colors: 26
                states: 35
                compiled_words: 782
                forced_words: 3,868
                compile_alloc_words: 3,202
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/32:
                colors: 26
                states: 35
                compiled_words: 782
                forced_words: 3,868
                compile_alloc_words: 3,202
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/32:
                colors: 27
                states: 67
                compiled_words: 1,140
                forced_words: 7,187
                compile_alloc_words: 3,723
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 479
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/32:
                colors: 28
                states: 36
                compiled_words: 822
                forced_words: 4,076
                compile_alloc_words: 5,455
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 640
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/256:
                colors: 64
                states: 515
                compiled_words: 3,703
                forced_words: 69,711
                compile_alloc_words: 12,625
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/256:
                colors: 64
                states: 515
                compiled_words: 3,703
                forced_words: 69,711
                compile_alloc_words: 12,625
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/256:
                colors: 64
                states: 515
                compiled_words: 6,301
                forced_words: 72,742
                compile_alloc_words: 15,192
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,879
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/256:
                colors: 64
                states: 517
                compiled_words: 3,731
                forced_words: 70,058
                compile_alloc_words: 29,092
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,440
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,783
                forced_words: 1,102,695
                compile_alloc_words: 131,665
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/quoted/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,783
                forced_words: 1,102,695
                compile_alloc_words: 131,665
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/mixed/4096:
                colors: 64
                states: 8,195
                compiled_words: 90,781
                forced_words: 1,151,782
                compile_alloc_words: 172,632
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,849
                construct_major_words: 1,040
                construct_promoted_words: 10
                forcing: full
              literal/alternation/4096:
                colors: 64
                states: 8,197
                compiled_words: 49,811
                forced_words: 1,104,002
                compile_alloc_words: 393,892
                compile_major_words: 164,172
                compile_promoted_words: 164,172
                construct_alloc_words: 49,388
                construct_major_words: 2,062
                construct_promoted_words: 10
                forcing: full
              literal/plain/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,063
                forced_words: 17,635,173
                compile_alloc_words: 2,036,305
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/quoted/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,063
                forced_words: 17,635,173
                compile_alloc_words: 2,036,305
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/mixed/65536:
                colors: 64
                states: 131,075
                compiled_words: 1,442,461
                forced_words: 18,416,422
                compile_alloc_words: 2,691,672
                compile_major_words: 1,568,909
                compile_promoted_words: 1,568,909
                construct_alloc_words: 393,489
                construct_major_words: 344,273
                construct_promoted_words: 327,883
                forcing: full
              literal/hit/32:
                colors: 26
                states: 35
                compiled_words: 782
                forced_words: 3,868
                compile_alloc_words: 3,202
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/32:
                colors: 26
                states: 35
                compiled_words: 782
                forced_words: 3,868
                compile_alloc_words: 3,202
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/32:
                colors: 26
                states: 35
                compiled_words: 782
                forced_words: 3,868
                compile_alloc_words: 3,202
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/32:
                colors: 26
                states: 35
                compiled_words: 781
                forced_words: 3,547
                compile_alloc_words: 2,998
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 425
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/256:
                colors: 64
                states: 515
                compiled_words: 3,703
                forced_words: 69,711
                compile_alloc_words: 12,625
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/256:
                colors: 64
                states: 515
                compiled_words: 3,703
                forced_words: 69,711
                compile_alloc_words: 12,625
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/256:
                colors: 64
                states: 515
                compiled_words: 3,703
                forced_words: 69,711
                compile_alloc_words: 12,625
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/256:
                colors: 64
                states: 259
                compiled_words: 3,702
                forced_words: 34,376
                compile_alloc_words: 12,307
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,825
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,783
                forced_words: 1,102,695
                compile_alloc_words: 131,665
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/absent/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,783
                forced_words: 1,102,695
                compile_alloc_words: 131,665
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/late miss/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,783
                forced_words: 1,102,695
                compile_alloc_words: 131,665
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/anchored/4096:
                colors: 64
                states: 4,099
                compiled_words: 49,782
                forced_words: 538,376
                compile_alloc_words: 131,347
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,797
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/overlap/256:
                colors: 3
                states: 1,021
                compiled_words: 3,329
                forced_words: 943,572
                compile_alloc_words: 8,856
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/capturing:
                colors: 48
                states: 6,947
                compiled_words: 383,321
                forced_words: 1,278,364
                compile_alloc_words: 1,034,744
                compile_major_words: 558,178
                compile_promoted_words: 558,178
                construct_alloc_words: 220,816
                construct_major_words: 9,880
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/noncapturing:
                colors: 48
                states: 6,947
                compiled_words: 106,033
                forced_words: 893,126
                compile_alloc_words: 1,369,620
                compile_major_words: 458,265
                compile_promoted_words: 458,265
                construct_alloc_words: 216,586
                construct_major_words: 10,586
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/capturing:
                colors: 42
                states: 375
                compiled_words: 17,983
                forced_words: 501,965
                compile_alloc_words: 64,373
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,040
                construct_major_words: 834
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/noncapturing:
                colors: 42
                states: 375
                compiled_words: 17,983
                forced_words: 501,965
                compile_alloc_words: 63,857
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 16,879
                construct_major_words: 1,028
                construct_promoted_words: 10
                forcing: full
              boolean/literal-short-hit:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-miss:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-late:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-overlap-miss:
                colors: 3
                states: 65
                compiled_words: 461
                forced_words: 7,858
                compile_alloc_words: 1,449
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/alt-long-miss:
                colors: 10
                states: 15
                compiled_words: 468
                forced_words: 1,559
                compile_alloc_words: 1,808
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/suffix-miss:
                colors: 4
                states: 5
                compiled_words: 311
                forced_words: 704
                compile_alloc_words: 1,119
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/star-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 270
                forced_words: 595
                compile_alloc_words: 914
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/plus-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 280
                forced_words: 598
                compile_alloc_words: 924
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-repeat:
                colors: 2
                states: 5
                compiled_words: 279
                forced_words: 558
                compile_alloc_words: 819
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class:
                colors: 2
                states: 5
                compiled_words: 279
                forced_words: 558
                compile_alloc_words: 846
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-varied:
                colors: 2
                states: 5
                compiled_words: 279
                forced_words: 558
                compile_alloc_words: 846
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-fail:
                colors: 2
                states: 5
                compiled_words: 279
                forced_words: 558
                compile_alloc_words: 846
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-hit:
                colors: 6
                states: 10
                compiled_words: 407
                forced_words: 1,120
                compile_alloc_words: 1,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-miss:
                colors: 6
                states: 10
                compiled_words: 407
                forced_words: 1,120
                compile_alloc_words: 1,651
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/http-hit:
                colors: 18
                states: 30
                compiled_words: 707
                forced_words: 3,414
                compile_alloc_words: 2,941
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-miss:
                colors: 6
                states: 12
                compiled_words: 389
                forced_words: 1,294
                compile_alloc_words: 1,441
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/line-anchor-miss:
                colors: 5
                states: 11
                compiled_words: 347
                forced_words: 1,158
                compile_alloc_words: 1,268
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/longest-hit:
                colors: 3
                states: 9
                compiled_words: 324
                forced_words: 948
                compile_alloc_words: 1,118
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/ambiguous-miss:
                colors: 3
                states: 7
                compiled_words: 316
                forced_words: 820
                compile_alloc_words: 1,153
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-reject:
                colors: 7
                states: 9
                compiled_words: 352
                forced_words: 906
                compile_alloc_words: 1,216
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-accept:
                colors: 7
                states: 9
                compiled_words: 352
                forced_words: 906
                compile_alloc_words: 1,216
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-candidates:
                colors: 3
                states: 259
                compiled_words: 3,329
                forced_words: 20,657
                compile_alloc_words: 8,856
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-short:
                colors: 3
                states: 11
                compiled_words: 353
                forced_words: 1,061
                compile_alloc_words: 1,168
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/cycle-anchored:
                colors: 3
                states: 6
                compiled_words: 307
                forced_words: 657
                compile_alloc_words: 925
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/class-late-hit:
                colors: 3
                states: 6
                compiled_words: 298
                forced_words: 734
                compile_alloc_words: 1,058
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-late-hit:
                colors: 6
                states: 12
                compiled_words: 389
                forced_words: 1,294
                compile_alloc_words: 1,441
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1k-miss:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1m-miss:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/path-glob:
                colors: 6
                states: 16
                compiled_words: 730
                forced_words: 2,753
                compile_alloc_words: 3,633
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/captures:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,191
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/no group:
                colors: 5
                states: 9
                compiled_words: 341
                forced_words: 932
                compile_alloc_words: 1,204
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/captures:
                colors: 3
                states: 125
                compiled_words: 641
                forced_words: 21,200
                compile_alloc_words: 1,899
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/no group:
                colors: 3
                states: 125
                compiled_words: 641
                forced_words: 21,200
                compile_alloc_words: 1,912
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/captures:
                colors: 6
                states: 11
                compiled_words: 479
                forced_words: 1,361
                compile_alloc_words: 1,728
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/no group:
                colors: 6
                states: 10
                compiled_words: 407
                forced_words: 1,120
                compile_alloc_words: 1,660
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/captures:
                colors: 18
                states: 30
                compiled_words: 779
                forced_words: 3,685
                compile_alloc_words: 3,072
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/no group:
                colors: 18
                states: 29
                compiled_words: 707
                forced_words: 3,334
                compile_alloc_words: 3,004
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/captures:
                colors: 3
                states: 10
                compiled_words: 388
                forced_words: 1,285
                compile_alloc_words: 1,230
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/no group:
                colors: 3
                states: 7
                compiled_words: 316
                forced_words: 820
                compile_alloc_words: 1,162
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/captures:
                colors: 6
                states: 12
                compiled_words: 413
                forced_words: 1,366
                compile_alloc_words: 1,458
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/no group:
                colors: 6
                states: 12
                compiled_words: 389
                forced_words: 1,294
                compile_alloc_words: 1,444
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/captures:
                colors: 2
                states: 35
                compiled_words: 1,850
                forced_words: 84,172
                compile_alloc_words: 4,313
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/no group:
                colors: 2
                states: 35
                compiled_words: 1,082
                forced_words: 5,685
                compile_alloc_words: 3,462
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/captures:
                colors: 2
                states: 35
                compiled_words: 1,466
                forced_words: 28,353
                compile_alloc_words: 3,929
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/no group:
                colors: 2
                states: 35
                compiled_words: 1,466
                forced_words: 28,353
                compile_alloc_words: 3,942
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/literal:
                colors: 9
                states: 11
                compiled_words: 390
                forced_words: 1,158
                compile_alloc_words: 1,504
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/greedy:
                colors: 3
                states: 25
                compiled_words: 324
                forced_words: 2,634
                compile_alloc_words: 1,077
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/lazy:
                colors: 3
                states: 11
                compiled_words: 324
                forced_words: 1,198
                compile_alloc_words: 1,091
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/captures:
                colors: 4
                states: 9
                compiled_words: 382
                forced_words: 1,053
                compile_alloc_words: 1,147
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/word boundaries:
                colors: 2
                states: 8
                compiled_words: 352
                forced_words: 944
                compile_alloc_words: 1,170
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/broad alternatives:
                colors: 12
                states: 34
                compiled_words: 2,220
                forced_words: 7,848
                compile_alloc_words: 7,492
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/fixed repetition:
                colors: 2
                states: 131
                compiled_words: 1,532
                forced_words: 9,549
                compile_alloc_words: 2,098
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/longest:
                colors: 2
                states: 21
                compiled_words: 316
                forced_words: 2,392
                compile_alloc_words: 1,032
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nullable:
                colors: 2
                states: 5
                compiled_words: 308
                forced_words: 729
                compile_alloc_words: 981
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nested erasure:
                colors: 3
                states: 9
                compiled_words: 490
                forced_words: 1,234
                compile_alloc_words: 1,179
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/capture loop:
                colors: 4
                states: 13
                compiled_words: 396
                forced_words: 1,485
                compile_alloc_words: 1,088
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/unanchored bytes:
                colors: 256
                states: 259
                compiled_words: 4,885
                forced_words: 92,143
                compile_alloc_words: 29,934
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/sequential32:
                colors: 2
                states: 35
                compiled_words: 1,402
                forced_words: 4,231
                compile_alloc_words: 2,713
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion email:
                colors: 6
                states: 35
                compiled_words: 408
                forced_words: 3,534
                compile_alloc_words: 1,712
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion http:
                colors: 16
                states: 27
                compiled_words: 713
                forced_words: 3,179
                compile_alloc_words: 2,776
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/tex:
                colors: 42
                states: 68
                compiled_words: 11,359
                forced_words: 81,982
                compile_alloc_words: 51,770
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              |}]
          ;;
        end
        in
        ()
    ;;
  end)
;;
