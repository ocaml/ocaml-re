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
                compiled_words: 494
                forced_words: 3,859
                compile_alloc_words: 1,489
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              lots of a's:
                colors: 3
                states: 9
                compiled_words: 310
                forced_words: 904
                compile_alloc_words: 1,053
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              media type match:
                colors: 3
                states: 10
                compiled_words: 319
                forced_words: 1,087
                compile_alloc_words: 1,138
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri:
                colors: 6
                states: 242
                compiled_words: 744
                forced_words: 36,759
                compile_alloc_words: 2,857
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              tex gitignore:
                colors: 42
                states: 68
                compiled_words: 11,362
                forced_words: 75,730
                compile_alloc_words: 51,760
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/no group:
                colors: 13
                states: 557
                compiled_words: 961
                forced_words: 43,707
                compile_alloc_words: 4,003
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/manual/group:
                colors: 13
                states: 374
                compiled_words: 841
                forced_words: 57,977
                compile_alloc_words: 3,881
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              http/auto/execp no group:
                colors: 13
                states: 799
                compiled_words: 1,500
                forced_words: 97,765
                compile_alloc_words: 4,571
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              http/auto/all_gen:
                colors: 13
                states: 707
                compiled_words: 1,280
                forced_words: 121,760
                compile_alloc_words: 4,349
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              string traversal from #210:
                colors: 3
                states: 65
                compiled_words: 464
                forced_words: 7,861
                compile_alloc_words: 1,439
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              kleene star compilation:
                colors: 2
                states: 5
                compiled_words: 273
                forced_words: 598
                compile_alloc_words: 904
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1:
                colors: 3
                states: 1,003
                compiled_words: 10,293
                forced_words: 2,061,343
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2:
                colors: 4
                states: 1,003
                compiled_words: 10,311
                forced_words: 4,564,600
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              repeated sequence re:
                colors: 256
                states: 12,803
                compiled_words: 130,328
                forced_words: 8,906,442
                compile_alloc_words: 157,963
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              split on whitespace:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 649
                compile_alloc_words: 954
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              shared prefixes:
                colors: 27
                states: 8
                compiled_words: 6,207
                forced_words: 10,408
                compile_alloc_words: 24,996
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/1:
                colors: 6
                states: 242
                compiled_words: 744
                forced_words: 36,759
                compile_alloc_words: 2,857
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/2:
                colors: 6
                states: 242
                compiled_words: 744
                forced_words: 36,759
                compile_alloc_words: 2,857
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              uri/input/3:
                colors: 6
                states: 242
                compiled_words: 744
                forced_words: 36,759
                compile_alloc_words: 2,857
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/email-html5:
                colors: 7
                states: 256
                compiled_words: 3,351
                forced_words: 28,679
                compile_alloc_words: 8,899
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/url:
                colors: 12
                states: 17
                compiled_words: 519
                forced_words: 2,055
                compile_alloc_words: 2,324
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv4:
                colors: 8
                states: 35
                compiled_words: 969
                forced_words: 4,849
                compile_alloc_words: 3,243
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/ipv6:
                colors: 16
                states: 1,244
                compiled_words: 11,203
                forced_words: 254,448
                compile_alloc_words: 23,243
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/mac:
                colors: 4
                states: 21
                compiled_words: 563
                forced_words: 2,428
                compile_alloc_words: 1,531
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-date:
                colors: 8
                states: 17
                compiled_words: 539
                forced_words: 1,972
                compile_alloc_words: 2,064
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/time-24h:
                colors: 8
                states: 13
                compiled_words: 421
                forced_words: 1,426
                compile_alloc_words: 1,758
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/iso-timestamp:
                colors: 9
                states: 31
                compiled_words: 669
                forced_words: 3,215
                compile_alloc_words: 2,814
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/phone-us:
                colors: 8
                states: 35
                compiled_words: 579
                forced_words: 3,557
                compile_alloc_words: 2,308
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/credit-card:
                colors: 11
                states: 66
                compiled_words: 1,014
                forced_words: 6,276
                compile_alloc_words: 2,971
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/uuid:
                colors: 6
                states: 40
                compiled_words: 653
                forced_words: 3,832
                compile_alloc_words: 2,099
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/semver:
                colors: 8
                states: 41
                compiled_words: 926
                forced_words: 5,211
                compile_alloc_words: 4,280
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/base64:
                colors: 4
                states: 12
                compiled_words: 416
                forced_words: 1,378
                compile_alloc_words: 1,741
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/validation/domain:
                colors: 6
                states: 316
                compiled_words: 4,794
                forced_words: 37,851
                compile_alloc_words: 8,065
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/apache-combined:
                colors: 8
                states: 82
                compiled_words: 963
                forced_words: 8,602
                compile_alloc_words: 3,311
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/syslog-rfc3164:
                colors: 13
                states: 33
                compiled_words: 785
                forced_words: 3,318
                compile_alloc_words: 3,110
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/log/structured-line:
                colors: 13
                states: 37
                compiled_words: 849
                forced_words: 3,291
                compile_alloc_words: 3,292
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/csv/rfc4180-row:
                colors: 5
                states: 16
                compiled_words: 484
                forced_words: 1,960
                compile_alloc_words: 2,103
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/string:
                colors: 8
                states: 12
                compiled_words: 438
                forced_words: 1,470
                compile_alloc_words: 1,865
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/json/number:
                colors: 8
                states: 14
                compiled_words: 498
                forced_words: 1,658
                compile_alloc_words: 2,076
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/tag:
                colors: 7
                states: 26
                compiled_words: 498
                forced_words: 2,797
                compile_alloc_words: 1,998
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/html/comment:
                colors: 5
                states: 15
                compiled_words: 375
                forced_words: 1,629
                compile_alloc_words: 1,352
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/markdown/link:
                colors: 7
                states: 17
                compiled_words: 574
                forced_words: 1,529
                compile_alloc_words: 2,334
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (impractical complete automaton)
              common/comment/c-style:
                colors: 3
                states: 27
                compiled_words: 429
                forced_words: 3,004
                compile_alloc_words: 1,662
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/keywords:
                colors: 26
                states: 225
                compiled_words: 3,807
                forced_words: 27,178
                compile_alloc_words: 16,564
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/lexer:
                colors: 19
                states: 246
                compiled_words: 1,122
                forced_words: 35,250
                compile_alloc_words: 6,796
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/code/number-literals:
                colors: 15
                states: 27
                compiled_words: 790
                forced_words: 3,378
                compile_alloc_words: 4,321
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/number/roman:
                colors: 9
                states: 30
                compiled_words: 820
                forced_words: 3,392
                compile_alloc_words: 2,665
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/money/usd:
                colors: 6
                states: 15
                compiled_words: 442
                forced_words: 1,644
                compile_alloc_words: 1,531
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/docker/reference:
                colors: 18
                states: 344
                compiled_words: 4,461
                forced_words: 43,136
                compile_alloc_words: 12,104
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/aws-access-key:
                colors: 9
                states: 5,473
                compiled_words: 611
                forced_words: 610,950
                compile_alloc_words: 2,286
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/secret/api-key-line:
                colors: 18
                states: 893
                compiled_words: 843
                forced_words: 95,723
                compile_alloc_words: 4,988
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/youtube/video-id:
                colors: 21
                states: 294
                compiled_words: 960
                forced_words: 33,079
                compile_alloc_words: 4,010
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              common/date/us-slash:
                colors: 8
                states: 95
                compiled_words: 651
                forced_words: 10,610
                compile_alloc_words: 2,837
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/seq-of-reps:
                colors: 10
                states: 83
                compiled_words: 723
                forced_words: 13,399
                compile_alloc_words: 2,126
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/capture-stars:
                colors: 6
                states: 305
                compiled_words: 788
                forced_words: 59,224
                compile_alloc_words: 1,825
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/star-plus:
                colors: 4
                states: 21
                compiled_words: 426
                forced_words: 2,307
                compile_alloc_words: 1,133
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-of-reps:
                colors: 6
                states: 22
                compiled_words: 474
                forced_words: 2,661
                compile_alloc_words: 1,445
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/flat-reps:
                colors: 5
                states: 17
                compiled_words: 616
                forced_words: 2,282
                compile_alloc_words: 1,442
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/word-digit:
                colors: 4
                states: 11
                compiled_words: 422
                forced_words: 1,250
                compile_alloc_words: 1,185
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/alt-literals:
                colors: 9
                states: 13
                compiled_words: 591
                forced_words: 1,769
                compile_alloc_words: 1,855
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              edge/nested-repetition/single-rep:
                colors: 4
                states: 13
                compiled_words: 368
                forced_words: 1,398
                compile_alloc_words: 1,031
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/whitespace/trim:
                colors: 3
                states: 8
                compiled_words: 340
                forced_words: 1,057
                compile_alloc_words: 1,236
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/whitespace/run:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 649
                compile_alloc_words: 968
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/list/tab-separated:
                colors: 3
                states: 47
                compiled_words: 412
                forced_words: 5,235
                compile_alloc_words: 1,244
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/list/separator-run:
                colors: 3
                states: 5
                compiled_words: 291
                forced_words: 661
                compile_alloc_words: 1,139
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/line/crlf:
                colors: 3
                states: 6
                compiled_words: 298
                forced_words: 809
                compile_alloc_words: 1,038
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/line/blank:
                colors: 3
                states: 9
                compiled_words: 297
                forced_words: 1,003
                compile_alloc_words: 1,124
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/comment/hash:
                colors: 4
                states: 11
                compiled_words: 334
                forced_words: 1,285
                compile_alloc_words: 1,332
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/integer:
                colors: 3
                states: 7
                compiled_words: 315
                forced_words: 863
                compile_alloc_words: 1,126
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/float:
                colors: 3
                states: 10
                compiled_words: 406
                forced_words: 1,377
                compile_alloc_words: 1,392
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/number/hex:
                colors: 4
                states: 8
                compiled_words: 319
                forced_words: 877
                compile_alloc_words: 1,195
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/config/key-value:
                colors: 4
                states: 13
                compiled_words: 430
                forced_words: 1,798
                compile_alloc_words: 1,718
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/config/colon-value:
                colors: 4
                states: 15
                compiled_words: 395
                forced_words: 2,256
                compile_alloc_words: 1,494
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/string/double-quoted:
                colors: 2
                states: 9
                compiled_words: 327
                forced_words: 1,062
                compile_alloc_words: 1,104
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/string/single-quoted:
                colors: 2
                states: 9
                compiled_words: 327
                forced_words: 1,062
                compile_alloc_words: 1,104
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/unit/duration:
                colors: 8
                states: 8
                compiled_words: 448
                forced_words: 1,145
                compile_alloc_words: 1,668
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/version/dotted:
                colors: 3
                states: 13
                compiled_words: 481
                forced_words: 1,707
                compile_alloc_words: 1,575
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/terminal/ansi:
                colors: 5
                states: 7
                compiled_words: 327
                forced_words: 846
                compile_alloc_words: 1,242
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/html/entity:
                colors: 9
                states: 10
                compiled_words: 429
                forced_words: 1,305
                compile_alloc_words: 1,689
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/template/opam:
                colors: 4
                states: 10
                compiled_words: 349
                forced_words: 1,146
                compile_alloc_words: 1,256
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              idiom/template/mustache:
                colors: 3
                states: 11
                compiled_words: 343
                forced_words: 1,256
                compile_alloc_words: 1,192
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/adjacent:
                colors: 8
                states: 8
                compiled_words: 368
                forced_words: 897
                compile_alloc_words: 1,444
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/interleaved:
                colors: 5
                states: 6
                compiled_words: 367
                forced_words: 759
                compile_alloc_words: 1,213
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/4:
                colors: 11
                states: 11
                compiled_words: 836
                forced_words: 1,975
                compile_alloc_words: 2,863
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/nested/16:
                colors: 12
                states: 26
                compiled_words: 2,282
                forced_words: 7,671
                compile_alloc_words: 7,401
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/log files:
                colors: 10
                states: 30
                compiled_words: 1,118
                forced_words: 4,024
                compile_alloc_words: 3,297
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/escape tokens:
                colors: 10
                states: 16
                compiled_words: 626
                forced_words: 2,024
                compile_alloc_words: 2,401
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              capture histories/routes:
                colors: 21
                states: 65
                compiled_words: 1,274
                forced_words: 8,480
                compile_alloc_words: 4,421
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              memory 1/10:
                colors: 3
                states: 11
                compiled_words: 10,293
                forced_words: 11,227
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/20:
                colors: 3
                states: 21
                compiled_words: 10,293
                forced_words: 12,247
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/40:
                colors: 3
                states: 41
                compiled_words: 10,293
                forced_words: 15,487
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/80:
                colors: 3
                states: 81
                compiled_words: 10,293
                forced_words: 26,767
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/100:
                colors: 3
                states: 101
                compiled_words: 10,293
                forced_words: 34,807
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 1/1000:
                colors: 3
                states: 1,001
                compiled_words: 10,293
                forced_words: 2,052,991
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/10:
                colors: 4
                states: 11
                compiled_words: 10,311
                forced_words: 11,437
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/20:
                colors: 4
                states: 21
                compiled_words: 10,311
                forced_words: 13,142
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/40:
                colors: 4
                states: 41
                compiled_words: 10,311
                forced_words: 19,252
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/80:
                colors: 4
                states: 81
                compiled_words: 10,311
                forced_words: 42,272
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/100:
                colors: 4
                states: 101
                compiled_words: 10,311
                forced_words: 59,182
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              memory 2/1000:
                colors: 4
                states: 1,001
                compiled_words: 10,311
                forced_words: 4,546,516
                compile_alloc_words: 11,186
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              expression IDs/broad/16/1024:
                colors: 9
                states: 1,033
                compiled_words: 11,056
                forced_words: 92,201
                compile_alloc_words: 14,070
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 801
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/broad/4096/16384:
                colors: 17
                states: 16,401
                compiled_words: 350,489
                forced_words: 2,035,466
                compile_alloc_words: 1,248,166
                compile_major_words: 407,092
                compile_promoted_words: 407,092
                construct_alloc_words: 500,327
                construct_major_words: 141,345
                construct_promoted_words: 141,345
                forcing: full
              expression IDs/broad/65536/262144:
                colors: 21
                states: 262,165
                compiled_words: 5,996,825
                forced_words: 34,507,002
                compile_alloc_words: 22,876,975
                compile_major_words: 8,771,131
                compile_promoted_words: 8,771,131
                construct_alloc_words: 11,666,969
                construct_major_words: 2,654,611
                construct_promoted_words: 2,654,611
                forcing: full
              expression IDs/narrow/1024:
                colors: 2
                states: 1,027
                compiled_words: 10,495
                forced_words: 74,512
                compile_alloc_words: 11,061
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              expression IDs/narrow/1000000:
                colors: 2
                states: 1,000,003
                compiled_words: 10,000,255
                forced_words: 72,024,560
                compile_alloc_words: 10,000,821
                compile_major_words: 9,829,801
                compile_promoted_words: 9,829,801
                construct_alloc_words: 37
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/tiny:
                colors: 2
                states: 4
                compiled_words: 266
                forced_words: 505
                compile_alloc_words: 873
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/class:
                colors: 2
                states: 5
                compiled_words: 306
                forced_words: 641
                compile_alloc_words: 920
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/pair:
                colors: 3
                states: 7
                compiled_words: 362
                forced_words: 924
                compile_alloc_words: 999
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/rotation:
                colors: 2
                states: 5
                compiled_words: 296
                forced_words: 615
                compile_alloc_words: 829
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/short:
                colors: 4
                states: 8
                compiled_words: 384
                forced_words: 1,082
                compile_alloc_words: 1,300
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/categories:
                colors: 2
                states: 5
                compiled_words: 296
                forced_words: 615
                compile_alloc_words: 870
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/boundaries:
                colors: 4
                states: 14
                compiled_words: 414
                forced_words: 1,602
                compile_alloc_words: 1,482
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/literal256:
                colors: 256
                states: 259
                compiled_words: 4,887
                forced_words: 91,080
                compile_alloc_words: 29,070
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/wide run:
                colors: 256
                states: 259
                compiled_words: 4,936
                forced_words: 83,444
                compile_alloc_words: 29,998
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/class run:
                colors: 2
                states: 5
                compiled_words: 306
                forced_words: 605
                compile_alloc_words: 922
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/search:
                colors: 5
                states: 8
                compiled_words: 354
                forced_words: 970
                compile_alloc_words: 1,359
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/alternating:
                colors: 3
                states: 9
                compiled_words: 422
                forced_words: 1,063
                compile_alloc_words: 1,185
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlapping alternatives:
                colors: 11
                states: 10
                compiled_words: 5,001
                forced_words: 15,248
                compile_alloc_words: 16,246
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/wide alphabet:
                colors: 256
                states: 7
                compiled_words: 7,008
                forced_words: 9,413
                compile_alloc_words: 46,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/token classifier:
                colors: 17
                states: 17
                compiled_words: 1,108
                forced_words: 2,631
                compile_alloc_words: 17,855
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/nested captures:
                colors: 2
                states: 9
                compiled_words: 362
                forced_words: 1,214
                compile_alloc_words: 1,108
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/repeated bytes4:
                colors: 256
                states: 1,027
                compiled_words: 12,568
                forced_words: 385,248
                compile_alloc_words: 40,203
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              expression IDs/broad/256/4096:
                colors: 13
                states: 4,109
                compiled_words: 51,352
                forced_words: 395,597
                compile_alloc_words: 97,617
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 20,094
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              automata/captures/sequential/16:
                colors: 2
                states: 19
                compiled_words: 829
                forced_words: 2,311
                compile_alloc_words: 1,740
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/16:
                colors: 2
                states: 35
                compiled_words: 836
                forced_words: 5,253
                compile_alloc_words: 1,771
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/16:
                colors: 2
                states: 35
                compiled_words: 849
                forced_words: 3,823
                compile_alloc_words: 1,789
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/128:
                colors: 2
                states: 131
                compiled_words: 4,861
                forced_words: 15,769
                compile_alloc_words: 8,572
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/128:
                colors: 2
                states: 259
                compiled_words: 4,868
                forced_words: 125,351
                compile_alloc_words: 8,603
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/128:
                colors: 2
                states: 259
                compiled_words: 4,881
                forced_words: 27,825
                compile_alloc_words: 8,621
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/sequential/512:
                colors: 2
                states: 515
                compiled_words: 18,685
                forced_words: 62,293
                compile_alloc_words: 31,996
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/repeated/512:
                colors: 2
                states: 1,027
                compiled_words: 18,692
                forced_words: 1,680,227
                compile_alloc_words: 32,027
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/captures/erase/512:
                colors: 2
                states: 1,027
                compiled_words: 18,705
                forced_words: 110,445
                compile_alloc_words: 32,045
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/128:
                colors: 255
                states: 131
                compiled_words: 2,582
                forced_words: 45,829
                compile_alloc_words: 82,808
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/256:
                colors: 255
                states: 259
                compiled_words: 4,118
                forced_words: 90,053
                compile_alloc_words: 94,950
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/512:
                colors: 255
                states: 515
                compiled_words: 7,190
                forced_words: 178,501
                compile_alloc_words: 121,481
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/overlap/2048:
                colors: 255
                states: 2,051
                compiled_words: 25,622
                forced_words: 709,189
                compile_alloc_words: 284,879
                compile_major_words: 18,847
                compile_promoted_words: 18,847
                forcing: full
              automata/pathological/100:
                colors: 3
                states: 103
                compiled_words: 1,293
                forced_words: 26,509
                compile_alloc_words: 2,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/pathological/1000:
                colors: 3
                states: 1,003
                compiled_words: 10,293
                forced_words: 2,061,343
                compile_alloc_words: 11,112
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              automata/occupancy/8:
                colors: 256
                states: 772
                compiled_words: 13,091
                forced_words: 272,824
                compile_alloc_words: 60,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/32:
                colors: 256
                states: 772
                compiled_words: 13,091
                forced_words: 272,824
                compile_alloc_words: 60,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/64:
                colors: 256
                states: 772
                compiled_words: 13,091
                forced_words: 272,824
                compile_alloc_words: 60,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/128:
                colors: 256
                states: 772
                compiled_words: 13,091
                forced_words: 272,824
                compile_alloc_words: 60,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/occupancy/256:
                colors: 256
                states: 772
                compiled_words: 13,091
                forced_words: 272,824
                compile_alloc_words: 60,737
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/1:
                colors: 256
                states: 2
                compiled_words: 4,902
                forced_words: 5,306
                compile_alloc_words: 30,008
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/8:
                colors: 256
                states: 2
                compiled_words: 4,902
                forced_words: 5,306
                compile_alloc_words: 30,008
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/32:
                colors: 256
                states: 2
                compiled_words: 4,902
                forced_words: 5,306
                compile_alloc_words: 30,008
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/0/128:
                colors: 256
                states: 2
                compiled_words: 4,902
                forced_words: 5,306
                compile_alloc_words: 30,008
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/1:
                colors: 256
                states: 2
                compiled_words: 4,916
                forced_words: 5,344
                compile_alloc_words: 30,031
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/8:
                colors: 256
                states: 2
                compiled_words: 4,916
                forced_words: 5,344
                compile_alloc_words: 30,031
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/32:
                colors: 256
                states: 2
                compiled_words: 4,916
                forced_words: 5,344
                compile_alloc_words: 30,031
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/1/128:
                colors: 256
                states: 2
                compiled_words: 4,916
                forced_words: 5,344
                compile_alloc_words: 30,031
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/1:
                colors: 256
                states: 2
                compiled_words: 4,988
                forced_words: 5,488
                compile_alloc_words: 30,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/8:
                colors: 256
                states: 2
                compiled_words: 4,988
                forced_words: 5,488
                compile_alloc_words: 30,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/32:
                colors: 256
                states: 2
                compiled_words: 4,988
                forced_words: 5,488
                compile_alloc_words: 30,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/4/128:
                colors: 256
                states: 2
                compiled_words: 4,988
                forced_words: 5,488
                compile_alloc_words: 30,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/1:
                colors: 256
                states: 2
                compiled_words: 5,276
                forced_words: 6,064
                compile_alloc_words: 30,918
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/8:
                colors: 256
                states: 2
                compiled_words: 5,276
                forced_words: 6,064
                compile_alloc_words: 30,918
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/32:
                colors: 256
                states: 2
                compiled_words: 5,276
                forced_words: 6,064
                compile_alloc_words: 30,918
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/16/128:
                colors: 256
                states: 2
                compiled_words: 5,276
                forced_words: 6,064
                compile_alloc_words: 30,918
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/1:
                colors: 256
                states: 2
                compiled_words: 6,428
                forced_words: 8,368
                compile_alloc_words: 33,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/8:
                colors: 256
                states: 2
                compiled_words: 6,428
                forced_words: 8,368
                compile_alloc_words: 33,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/32:
                colors: 256
                states: 2
                compiled_words: 6,428
                forced_words: 8,368
                compile_alloc_words: 33,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/duplicate status/64/128:
                colors: 256
                states: 2
                compiled_words: 6,428
                forced_words: 8,368
                compile_alloc_words: 33,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/1:
                colors: 23
                states: 189
                compiled_words: 6,831
                forced_words: 37,961
                compile_alloc_words: 13,523
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/16/8:
                colors: 23
                states: 189
                compiled_words: 6,831
                forced_words: 37,961
                compile_alloc_words: 13,523
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/1:
                colors: 26
                states: 332
                compiled_words: 52,600
                forced_words: 257,648
                compile_alloc_words: 98,955
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/128/8:
                colors: 26
                states: 332
                compiled_words: 52,600
                forced_words: 257,648
                compile_alloc_words: 98,955
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/descriptor width/1024/1:
                colors: 29
                states: 745
                compiled_words: 422,776
                forced_words: 2,347,901
                compile_alloc_words: 814,839
                compile_major_words: 489,829
                compile_promoted_words: 489,829
                forcing: full
              automata/descriptor width/1024/8:
                colors: 29
                states: 745
                compiled_words: 422,776
                forced_words: 2,347,901
                compile_alloc_words: 814,839
                compile_major_words: 489,829
                compile_promoted_words: 489,829
                forcing: full
              automata/descriptor width/4096/1:
                colors: 31
                states: 1,883
                compiled_words: 1,702,264
                forced_words: 10,529,047
                compile_alloc_words: 3,365,325
                compile_major_words: 2,001,357
                compile_promoted_words: 2,001,357
                forcing: full
              automata/descriptor width/4096/8:
                colors: 31
                states: 1,883
                compiled_words: 1,702,264
                forced_words: 10,529,047
                compile_alloc_words: 3,365,325
                compile_major_words: 2,001,357
                compile_promoted_words: 2,001,357
                forcing: full
              automata/interleaved/1:
                colors: 5
                states: 8
                compiled_words: 439
                forced_words: 985
                compile_alloc_words: 1,395
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/4:
                colors: 11
                states: 11
                compiled_words: 836
                forced_words: 1,975
                compile_alloc_words: 2,863
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/8:
                colors: 12
                states: 16
                compiled_words: 1,322
                forced_words: 3,572
                compile_alloc_words: 4,417
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/16:
                colors: 12
                states: 26
                compiled_words: 2,282
                forced_words: 7,671
                compile_alloc_words: 7,401
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              automata/interleaved/32:
                colors: 12
                states: 45
                compiled_words: 4,202
                forced_words: 19,660
                compile_alloc_words: 13,369
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              parser/empty:
                colors: 1
                states: 2
                compiled_words: 248
                forced_words: 367
                compile_alloc_words: 832
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/one byte:
                colors: 2
                states: 4
                compiled_words: 266
                forced_words: 505
                compile_alloc_words: 873
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 212
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/short literal:
                colors: 13
                states: 37
                compiled_words: 519
                forced_words: 3,970
                compile_alloc_words: 2,009
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 311
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/small alternation:
                colors: 2
                states: 4
                compiled_words: 266
                forced_words: 505
                compile_alloc_words: 941
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 256
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/captures:
                colors: 2
                states: 131
                compiled_words: 4,862
                forced_words: 252,749
                compile_alloc_words: 8,677
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,355
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/noncapturing:
                colors: 3
                states: 259
                compiled_words: 3,332
                forced_words: 209,418
                compile_alloc_words: 12,302
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,475
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/empty groups:
                colors: 1
                states: 2
                compiled_words: 3,320
                forced_words: 5,999
                compile_alloc_words: 8,128
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 2,323
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/classes:
                colors: 2
                states: 131
                compiled_words: 1,790
                forced_words: 84,645
                compile_alloc_words: 10,275
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,235
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/ranges:
                colors: 2
                states: 131
                compiled_words: 1,790
                forced_words: 84,645
                compile_alloc_words: 13,887
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 11,091
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/escapes:
                colors: 4
                states: 8
                compiled_words: 14,577
                forced_words: 15,318
                compile_alloc_words: 47,278
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 5,971
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              parser/numeric escapes:
                colors: 5
                states: 515
                compiled_words: 6,416
                forced_words: 335,807
                compile_alloc_words: 16,888
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 9,039
                construct_major_words: 686
                construct_promoted_words: 10
                forcing: full
              parser/named groups:
                colors: 2
                states: 131
                compiled_words: 6,014
                forced_words: 253,901
                compile_alloc_words: 9,829
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,919
                construct_major_words: 526
                construct_promoted_words: 10
                forcing: full
              parser/long name:
                colors: 2
                states: 4
                compiled_words: 810
                forced_words: 1,074
                compile_alloc_words: 912
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 228
                construct_major_words: 1,552
                construct_promoted_words: 10
                forcing: full
              parser/comment 4K:
                colors: 1
                states: 2
                compiled_words: 248
                forced_words: 367
                compile_alloc_words: 832
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              parser/comment 64K:
                colors: 1
                states: 2
                compiled_words: 248
                forced_words: 367
                compile_alloc_words: 832
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 208
                construct_major_words: 16,398
                construct_promoted_words: 10
                forcing: full
              parser/quoted backslashes:
                colors: 5
                states: 3,075
                compiled_words: 37,136
                forced_words: 7,346,019
                compile_alloc_words: 96,246
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,639
                construct_major_words: 782
                construct_promoted_words: 10
                forcing: full
              parser/quantifiers:
                colors: 5
                states: 1
                compiled_words: 18,704
                forced_words: 18,761
                compile_alloc_words: 51,704
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 7,859
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              nested groups/capturing/64:
                colors: 2
                states: 67
                compiled_words: 1,022
                forced_words: 24,100
                compile_alloc_words: 3,058
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/64:
                colors: 2
                states: 67
                compiled_words: 1,022
                forced_words: 24,100
                compile_alloc_words: 2,866
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/128:
                colors: 2
                states: 131
                compiled_words: 1,790
                forced_words: 84,645
                compile_alloc_words: 5,234
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/128:
                colors: 2
                states: 131
                compiled_words: 1,790
                forced_words: 84,645
                compile_alloc_words: 4,850
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/256:
                colors: 2
                states: 259
                compiled_words: 3,326
                forced_words: 316,455
                compile_alloc_words: 9,586
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/256:
                colors: 2
                states: 259
                compiled_words: 3,326
                forced_words: 316,455
                compile_alloc_words: 8,818
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/512:
                colors: 2
                states: 515
                compiled_words: 6,398
                forced_words: 1,222,443
                compile_alloc_words: 18,290
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/512:
                colors: 2
                states: 515
                compiled_words: 6,398
                forced_words: 1,222,443
                compile_alloc_words: 16,754
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/capturing/1024:
                colors: 2
                states: 1,027
                compiled_words: 12,542
                forced_words: 4,803,891
                compile_alloc_words: 35,698
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              nested groups/plain/1024:
                colors: 2
                states: 1,027
                compiled_words: 12,542
                forced_words: 4,803,891
                compile_alloc_words: 32,626
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              literal/plain/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,871
                compile_alloc_words: 3,205
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,871
                compile_alloc_words: 3,205
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/32:
                colors: 27
                states: 67
                compiled_words: 1,143
                forced_words: 7,190
                compile_alloc_words: 3,726
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 479
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/32:
                colors: 28
                states: 36
                compiled_words: 825
                forced_words: 4,079
                compile_alloc_words: 5,458
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 640
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/256:
                colors: 64
                states: 515
                compiled_words: 3,706
                forced_words: 69,714
                compile_alloc_words: 12,628
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/quoted/256:
                colors: 64
                states: 515
                compiled_words: 3,706
                forced_words: 69,714
                compile_alloc_words: 12,628
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/mixed/256:
                colors: 64
                states: 515
                compiled_words: 6,304
                forced_words: 72,745
                compile_alloc_words: 15,195
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,879
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/alternation/256:
                colors: 64
                states: 517
                compiled_words: 3,734
                forced_words: 70,061
                compile_alloc_words: 29,095
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 3,440
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/plain/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,786
                forced_words: 1,102,698
                compile_alloc_words: 131,668
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/quoted/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,786
                forced_words: 1,102,698
                compile_alloc_words: 131,668
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/mixed/4096:
                colors: 64
                states: 8,195
                compiled_words: 90,784
                forced_words: 1,151,785
                compile_alloc_words: 172,635
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,849
                construct_major_words: 1,040
                construct_promoted_words: 10
                forcing: full
              literal/alternation/4096:
                colors: 64
                states: 8,197
                compiled_words: 49,814
                forced_words: 1,104,005
                compile_alloc_words: 393,895
                compile_major_words: 164,172
                compile_promoted_words: 164,172
                construct_alloc_words: 49,388
                construct_major_words: 2,062
                construct_promoted_words: 10
                forcing: full
              literal/plain/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,066
                forced_words: 17,635,176
                compile_alloc_words: 2,036,308
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/quoted/65536:
                colors: 64
                states: 131,075
                compiled_words: 787,066
                forced_words: 17,635,176
                compile_alloc_words: 2,036,308
                compile_major_words: 913,411
                compile_promoted_words: 913,411
                construct_alloc_words: 393,425
                construct_major_words: 344,264
                construct_promoted_words: 327,876
                forcing: full
              literal/mixed/65536:
                colors: 64
                states: 131,075
                compiled_words: 1,442,464
                forced_words: 18,416,425
                compile_alloc_words: 2,691,675
                compile_major_words: 1,568,909
                compile_promoted_words: 1,568,909
                construct_alloc_words: 393,489
                construct_major_words: 344,273
                construct_promoted_words: 327,883
                forcing: full
              literal/hit/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,871
                compile_alloc_words: 3,205
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,871
                compile_alloc_words: 3,205
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/32:
                colors: 26
                states: 35
                compiled_words: 785
                forced_words: 3,871
                compile_alloc_words: 3,205
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 413
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/32:
                colors: 26
                states: 35
                compiled_words: 784
                forced_words: 3,550
                compile_alloc_words: 3,001
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 425
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/256:
                colors: 64
                states: 515
                compiled_words: 3,706
                forced_words: 69,714
                compile_alloc_words: 12,628
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/absent/256:
                colors: 64
                states: 515
                compiled_words: 3,706
                forced_words: 69,714
                compile_alloc_words: 12,628
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/late miss/256:
                colors: 64
                states: 515
                compiled_words: 3,706
                forced_words: 69,714
                compile_alloc_words: 12,628
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/anchored/256:
                colors: 64
                states: 259
                compiled_words: 3,705
                forced_words: 34,379
                compile_alloc_words: 12,310
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,825
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              literal/hit/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,786
                forced_words: 1,102,698
                compile_alloc_words: 131,668
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/absent/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,786
                forced_words: 1,102,698
                compile_alloc_words: 131,668
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/late miss/4096:
                colors: 64
                states: 8,195
                compiled_words: 49,786
                forced_words: 1,102,698
                compile_alloc_words: 131,668
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,785
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/anchored/4096:
                colors: 64
                states: 4,099
                compiled_words: 49,785
                forced_words: 538,379
                compile_alloc_words: 131,350
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 24,797
                construct_major_words: 1,038
                construct_promoted_words: 10
                forcing: full
              literal/overlap/256:
                colors: 3
                states: 1,021
                compiled_words: 3,332
                forced_words: 943,575
                compile_alloc_words: 8,859
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 1,813
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/capturing:
                colors: 48
                states: 6,947
                compiled_words: 383,324
                forced_words: 1,272,157
                compile_alloc_words: 1,034,747
                compile_major_words: 558,178
                compile_promoted_words: 558,178
                construct_alloc_words: 220,816
                construct_major_words: 9,880
                construct_promoted_words: 10
                forcing: full
              gitignore/joomla/noncapturing:
                colors: 48
                states: 6,947
                compiled_words: 106,036
                forced_words: 893,111
                compile_alloc_words: 1,369,623
                compile_major_words: 470,378
                compile_promoted_words: 470,378
                construct_alloc_words: 216,586
                construct_major_words: 10,586
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/capturing:
                colors: 42
                states: 375
                compiled_words: 17,986
                forced_words: 496,199
                compile_alloc_words: 64,376
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 18,040
                construct_major_words: 834
                construct_promoted_words: 10
                forcing: full
              gitignore/latex-ignore-suffixes/noncapturing:
                colors: 42
                states: 375
                compiled_words: 17,986
                forced_words: 496,199
                compile_alloc_words: 63,860
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 16,879
                construct_major_words: 1,028
                construct_promoted_words: 10
                forcing: full
              boolean/literal-short-hit:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-miss:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-long-late:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-overlap-miss:
                colors: 3
                states: 65
                compiled_words: 464
                forced_words: 7,861
                compile_alloc_words: 1,452
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/alt-long-miss:
                colors: 10
                states: 15
                compiled_words: 471
                forced_words: 1,562
                compile_alloc_words: 1,811
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/suffix-miss:
                colors: 4
                states: 5
                compiled_words: 314
                forced_words: 707
                compile_alloc_words: 1,122
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/star-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 273
                forced_words: 598
                compile_alloc_words: 917
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/plus-greedy-hit:
                colors: 2
                states: 5
                compiled_words: 283
                forced_words: 601
                compile_alloc_words: 927
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-repeat:
                colors: 2
                states: 5
                compiled_words: 282
                forced_words: 561
                compile_alloc_words: 822
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class:
                colors: 2
                states: 5
                compiled_words: 282
                forced_words: 561
                compile_alloc_words: 849
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-varied:
                colors: 2
                states: 5
                compiled_words: 282
                forced_words: 561
                compile_alloc_words: 849
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/anchored-class-fail:
                colors: 2
                states: 5
                compiled_words: 282
                forced_words: 561
                compile_alloc_words: 849
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-hit:
                colors: 6
                states: 10
                compiled_words: 410
                forced_words: 1,123
                compile_alloc_words: 1,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/email-miss:
                colors: 6
                states: 10
                compiled_words: 410
                forced_words: 1,123
                compile_alloc_words: 1,654
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/http-hit:
                colors: 18
                states: 30
                compiled_words: 710
                forced_words: 3,417
                compile_alloc_words: 2,944
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-miss:
                colors: 6
                states: 12
                compiled_words: 392
                forced_words: 1,297
                compile_alloc_words: 1,444
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/line-anchor-miss:
                colors: 5
                states: 11
                compiled_words: 350
                forced_words: 1,161
                compile_alloc_words: 1,271
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/longest-hit:
                colors: 3
                states: 9
                compiled_words: 327
                forced_words: 942
                compile_alloc_words: 1,121
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/ambiguous-miss:
                colors: 3
                states: 7
                compiled_words: 319
                forced_words: 814
                compile_alloc_words: 1,156
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-reject:
                colors: 7
                states: 9
                compiled_words: 355
                forced_words: 909
                compile_alloc_words: 1,219
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/short-accept:
                colors: 7
                states: 9
                compiled_words: 355
                forced_words: 909
                compile_alloc_words: 1,219
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-candidates:
                colors: 3
                states: 259
                compiled_words: 3,332
                forced_words: 20,660
                compile_alloc_words: 8,859
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-dense-short:
                colors: 3
                states: 11
                compiled_words: 356
                forced_words: 1,064
                compile_alloc_words: 1,171
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/cycle-anchored:
                colors: 3
                states: 6
                compiled_words: 310
                forced_words: 660
                compile_alloc_words: 928
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/class-late-hit:
                colors: 3
                states: 6
                compiled_words: 301
                forced_words: 737
                compile_alloc_words: 1,061
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/word-boundary-late-hit:
                colors: 6
                states: 12
                compiled_words: 392
                forced_words: 1,297
                compile_alloc_words: 1,444
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1k-miss:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/literal-1m-miss:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/path-glob:
                colors: 6
                states: 16
                compiled_words: 733
                forced_words: 2,702
                compile_alloc_words: 3,636
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/captures:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,194
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/literal/no group:
                colors: 5
                states: 9
                compiled_words: 344
                forced_words: 935
                compile_alloc_words: 1,207
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/captures:
                colors: 3
                states: 125
                compiled_words: 644
                forced_words: 21,203
                compile_alloc_words: 1,902
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/overlap/no group:
                colors: 3
                states: 125
                compiled_words: 644
                forced_words: 21,203
                compile_alloc_words: 1,915
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/captures:
                colors: 6
                states: 11
                compiled_words: 482
                forced_words: 1,364
                compile_alloc_words: 1,731
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/email/no group:
                colors: 6
                states: 10
                compiled_words: 410
                forced_words: 1,123
                compile_alloc_words: 1,663
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/captures:
                colors: 18
                states: 30
                compiled_words: 782
                forced_words: 3,688
                compile_alloc_words: 3,075
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/http/no group:
                colors: 18
                states: 29
                compiled_words: 710
                forced_words: 3,337
                compile_alloc_words: 3,007
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/captures:
                colors: 3
                states: 10
                compiled_words: 391
                forced_words: 1,252
                compile_alloc_words: 1,233
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/ambiguous/no group:
                colors: 3
                states: 7
                compiled_words: 319
                forced_words: 814
                compile_alloc_words: 1,165
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/captures:
                colors: 6
                states: 12
                compiled_words: 416
                forced_words: 1,369
                compile_alloc_words: 1,461
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/boundaries/no group:
                colors: 6
                states: 12
                compiled_words: 392
                forced_words: 1,297
                compile_alloc_words: 1,447
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/captures:
                colors: 2
                states: 35
                compiled_words: 1,853
                forced_words: 84,175
                compile_alloc_words: 4,316
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-groups/no group:
                colors: 2
                states: 35
                compiled_words: 1,085
                forced_words: 5,409
                compile_alloc_words: 3,465
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/captures:
                colors: 2
                states: 35
                compiled_words: 1,469
                forced_words: 28,356
                compile_alloc_words: 3,932
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              histories/many-marks/no group:
                colors: 2
                states: 35
                compiled_words: 1,469
                forced_words: 28,356
                compile_alloc_words: 3,945
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/literal:
                colors: 9
                states: 11
                compiled_words: 393
                forced_words: 1,161
                compile_alloc_words: 1,507
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/greedy:
                colors: 3
                states: 25
                compiled_words: 327
                forced_words: 2,637
                compile_alloc_words: 1,080
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/lazy:
                colors: 3
                states: 11
                compiled_words: 327
                forced_words: 1,201
                compile_alloc_words: 1,094
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/captures:
                colors: 4
                states: 9
                compiled_words: 385
                forced_words: 1,056
                compile_alloc_words: 1,150
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/word boundaries:
                colors: 2
                states: 8
                compiled_words: 355
                forced_words: 947
                compile_alloc_words: 1,173
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/broad alternatives:
                colors: 12
                states: 34
                compiled_words: 2,223
                forced_words: 7,851
                compile_alloc_words: 7,495
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/fixed repetition:
                colors: 2
                states: 131
                compiled_words: 1,535
                forced_words: 9,552
                compile_alloc_words: 2,101
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/longest:
                colors: 2
                states: 21
                compiled_words: 319
                forced_words: 2,395
                compile_alloc_words: 1,035
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nullable:
                colors: 2
                states: 5
                compiled_words: 311
                forced_words: 732
                compile_alloc_words: 984
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/nested erasure:
                colors: 3
                states: 9
                compiled_words: 493
                forced_words: 1,237
                compile_alloc_words: 1,182
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/capture loop:
                colors: 4
                states: 13
                compiled_words: 399
                forced_words: 1,488
                compile_alloc_words: 1,091
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/unanchored bytes:
                colors: 256
                states: 259
                compiled_words: 4,888
                forced_words: 92,146
                compile_alloc_words: 29,937
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/sequential32:
                colors: 2
                states: 35
                compiled_words: 1,405
                forced_words: 4,234
                compile_alloc_words: 2,716
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion email:
                colors: 6
                states: 35
                compiled_words: 411
                forced_words: 3,537
                compile_alloc_words: 1,715
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              matching/promotion http:
                colors: 16
                states: 27
                compiled_words: 716
                forced_words: 3,182
                compile_alloc_words: 2,779
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: full
              boolean/tex:
                colors: 42
                states: 68
                compiled_words: 11,362
                forced_words: 75,730
                compile_alloc_words: 51,773
                compile_major_words: 10
                compile_promoted_words: 10
                forcing: inputs (exponential complete automaton)
              words/dictionary/250:
                colors: 27
                states: 8,135
                compiled_words: 23,755
                forced_words: 974,180
                compile_alloc_words: 89,602
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 8,242
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/500:
                colors: 27
                states: 19,768
                compiled_words: 45,307
                forced_words: 2,402,628
                compile_alloc_words: 180,261
                compile_major_words: 10
                compile_promoted_words: 10
                construct_alloc_words: 16,542
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/1000/whole-string:
                colors: 27
                states: 6,034
                compiled_words: 83,438
                forced_words: 608,500
                compile_alloc_words: 353,467
                compile_major_words: 139,804
                compile_promoted_words: 139,804
                construct_alloc_words: 32,403
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/2500/whole-string:
                colors: 27
                states: 14,174
                compiled_words: 197,702
                forced_words: 1,430,105
                compile_alloc_words: 901,184
                compile_major_words: 349,197
                compile_promoted_words: 349,197
                construct_alloc_words: 81,630
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/5000/whole-string:
                colors: 27
                states: 26,331
                compiled_words: 371,918
                forced_words: 2,663,095
                compile_alloc_words: 1,818,891
                compile_major_words: 695,989
                compile_promoted_words: 695,989
                construct_alloc_words: 162,656
                construct_major_words: 10
                construct_promoted_words: 10
                forcing: full
              words/dictionary/5000/word-boundary:
                colors: 28
                states: 26,334
                compiled_words: 371,967
                forced_words: 3,163,798
                compile_alloc_words: 1,819,298
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
