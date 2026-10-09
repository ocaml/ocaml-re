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
                forced_words: 4,782
                forcing: full
              lots of a's:
                colors: 3
                states: 9
                compiled_words: 307
                forced_words: 955
                forcing: full
              media type match:
                colors: 3
                states: 10
                compiled_words: 316
                forced_words: 1,130
                forcing: full
              uri:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 38,630
                forcing: full
              tex gitignore:
                colors: 42
                states: 68
                compiled_words: 11,359
                forced_words: 108,456
                forcing: inputs (exponential complete automaton)
              http/manual/no group:
                colors: 13
                states: 557
                compiled_words: 958
                forced_words: 52,906
                forcing: inputs (exponential complete automaton)
              http/manual/group:
                colors: 13
                states: 374
                compiled_words: 838
                forced_words: 69,592
                forcing: full
              http/auto/execp no group:
                colors: 13
                states: 799
                compiled_words: 1,497
                forced_words: 122,013
                forcing: inputs (exponential complete automaton)
              http/auto/all_gen:
                colors: 13
                states: 707
                compiled_words: 1,277
                forced_words: 146,683
                forcing: full
              string traversal from #210:
                colors: 3
                states: 65
                compiled_words: 461
                forced_words: 9,620
                forcing: full
              kleene star compilation:
                colors: 2
                states: 5
                compiled_words: 270
                forced_words: 613
                forcing: full
              memory 1:
                colors: 3
                states: 1,003
                compiled_words: 10,290
                forced_words: 3,074,364
                forcing: inputs (exponential complete automaton)
              memory 2:
                colors: 4
                states: 1,003
                compiled_words: 10,308
                forced_words: 6,578,619
                forcing: inputs (exponential complete automaton)
              repeated sequence re:
                colors: 256
                states: 12,803
                compiled_words: 130,325
                forced_words: 10,912,985
                forcing: full
              split on whitespace:
                colors: 2
                states: 5
                compiled_words: 280
                forced_words: 668
                forcing: full
              shared prefixes:
                colors: 27
                states: 8
                compiled_words: 6,204
                forced_words: 12,754
                forcing: full
              uri/input/1:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 38,630
                forcing: full
              uri/input/2:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 38,630
                forcing: full
              uri/input/3:
                colors: 6
                states: 242
                compiled_words: 741
                forced_words: 38,630
                forcing: full
              common/validation/email-html5:
                colors: 7
                states: 256
                compiled_words: 3,348
                forced_words: 31,160
                forcing: full
              common/validation/url:
                colors: 12
                states: 17
                compiled_words: 516
                forced_words: 2,124
                forcing: full
              common/validation/ipv4:
                colors: 8
                states: 35
                compiled_words: 966
                forced_words: 5,398
                forcing: full
              common/validation/ipv6:
                colors: 16
                states: 1,244
                compiled_words: 11,200
                forced_words: 283,017
                forcing: full
              common/validation/mac:
                colors: 4
                states: 21
                compiled_words: 560
                forced_words: 2,589
                forcing: full
              common/validation/iso-date:
                colors: 8
                states: 17
                compiled_words: 536
                forced_words: 2,057
                forcing: full
              common/validation/time-24h:
                colors: 8
                states: 13
                compiled_words: 418
                forced_words: 1,473
                forcing: full
              common/validation/iso-timestamp:
                colors: 9
                states: 31
                compiled_words: 666
                forced_words: 3,358
                forcing: full
              common/validation/phone-us:
                colors: 8
                states: 35
                compiled_words: 576
                forced_words: 3,777
                forcing: full
              common/validation/credit-card:
                colors: 11
                states: 66
                compiled_words: 1,011
                forced_words: 6,679
                forcing: full
              common/validation/uuid:
                colors: 6
                states: 40
                compiled_words: 650
                forced_words: 4,029
                forcing: full
              common/validation/semver:
                colors: 8
                states: 41
                compiled_words: 923
                forced_words: 5,570
                forcing: full
              common/validation/base64:
                colors: 4
                states: 12
                compiled_words: 413
                forced_words: 1,481
                forcing: full
              common/validation/domain:
                colors: 6
                states: 316
                compiled_words: 4,791
                forced_words: 42,174
                forcing: full
              common/log/apache-combined:
                colors: 8
                states: 82
                compiled_words: 960
                forced_words: 9,079
                forcing: full
              common/log/syslog-rfc3164:
                colors: 13
                states: 33
                compiled_words: 782
                forced_words: 3,461
                forcing: full
              common/log/structured-line:
                colors: 13
                states: 37
                compiled_words: 846
                forced_words: 3,706
                forcing: inputs (impractical complete automaton)
              common/csv/rfc4180-row:
                colors: 5
                states: 16
                compiled_words: 481
                forced_words: 2,059
                forcing: full
              common/json/string:
                colors: 8
                states: 12
                compiled_words: 435
                forced_words: 1,577
                forcing: full
              common/json/number:
                colors: 8
                states: 14
                compiled_words: 495
                forced_words: 1,715
                forcing: full
              common/html/tag:
                colors: 7
                states: 26
                compiled_words: 495
                forced_words: 3,059
                forcing: full
              common/html/comment:
                colors: 5
                states: 15
                compiled_words: 372
                forced_words: 1,776
                forcing: full
              common/markdown/link:
                colors: 7
                states: 17
                compiled_words: 571
                forced_words: 1,676
                forcing: inputs (impractical complete automaton)
              common/comment/c-style:
                colors: 3
                states: 27
                compiled_words: 426
                forced_words: 3,391
                forcing: full
              common/code/keywords:
                colors: 26
                states: 225
                compiled_words: 3,804
                forced_words: 29,387
                forcing: full
              common/code/lexer:
                colors: 19
                states: 246
                compiled_words: 1,119
                forced_words: 39,204
                forcing: full
              common/code/number-literals:
                colors: 15
                states: 27
                compiled_words: 787
                forced_words: 3,542
                forcing: full
              common/number/roman:
                colors: 9
                states: 30
                compiled_words: 817
                forced_words: 3,578
                forcing: full
              common/money/usd:
                colors: 6
                states: 15
                compiled_words: 439
                forced_words: 1,713
                forcing: full
              common/docker/reference:
                colors: 18
                states: 344
                compiled_words: 4,458
                forced_words: 46,282
                forcing: full
              common/secret/aws-access-key:
                colors: 9
                states: 5,473
                compiled_words: 608
                forced_words: 731,003
                forcing: full
              common/secret/api-key-line:
                colors: 18
                states: 893
                compiled_words: 840
                forced_words: 107,726
                forcing: full
              common/youtube/video-id:
                colors: 21
                states: 294
                compiled_words: 957
                forced_words: 36,932
                forcing: full
              common/date/us-slash:
                colors: 8
                states: 95
                compiled_words: 648
                forced_words: 12,234
                forcing: full
              edge/nested-repetition/seq-of-reps:
                colors: 10
                states: 83
                compiled_words: 720
                forced_words: 14,782
                forcing: full
              edge/nested-repetition/capture-stars:
                colors: 6
                states: 305
                compiled_words: 785
                forced_words: 65,569
                forcing: full
              edge/nested-repetition/star-plus:
                colors: 4
                states: 21
                compiled_words: 423
                forced_words: 2,486
                forcing: full
              edge/nested-repetition/alt-of-reps:
                colors: 6
                states: 22
                compiled_words: 471
                forced_words: 2,876
                forcing: full
              edge/nested-repetition/flat-reps:
                colors: 5
                states: 17
                compiled_words: 613
                forced_words: 2,447
                forcing: full
              edge/nested-repetition/word-digit:
                colors: 4
                states: 11
                compiled_words: 419
                forced_words: 1,323
                forcing: full
              edge/nested-repetition/alt-literals:
                colors: 9
                states: 13
                compiled_words: 588
                forced_words: 1,870
                forcing: full
              edge/nested-repetition/single-rep:
                colors: 4
                states: 13
                compiled_words: 365
                forced_words: 1,483
                forcing: full
              capture histories/adjacent:
                colors: 8
                states: 8
                compiled_words: 365
                forced_words: 948
                forcing: full
              capture histories/interleaved:
                colors: 5
                states: 6
                compiled_words: 364
                forced_words: 782
                forcing: full
              capture histories/nested/4:
                colors: 11
                states: 11
                compiled_words: 833
                forced_words: 2,106
                forcing: full
              capture histories/nested/16:
                colors: 12
                states: 26
                compiled_words: 2,279
                forced_words: 8,376
                forcing: full
              capture histories/log files:
                colors: 10
                states: 30
                compiled_words: 1,115
                forced_words: 4,393
                forcing: full
              capture histories/escape tokens:
                colors: 10
                states: 16
                compiled_words: 623
                forced_words: 2,139
                forcing: full
              capture histories/routes:
                colors: 21
                states: 65
                compiled_words: 1,271
                forced_words: 9,295
                forcing: full
              memory 1/10:
                colors: 3
                states: 11
                compiled_words: 10,290
                forced_words: 11,416
                forcing: inputs (exponential complete automaton)
              memory 1/20:
                colors: 3
                states: 21
                compiled_words: 10,290
                forced_words: 12,826
                forcing: inputs (exponential complete automaton)
              memory 1/40:
                colors: 3
                states: 41
                compiled_words: 10,290
                forced_words: 17,446
                forcing: inputs (exponential complete automaton)
              memory 1/80:
                colors: 3
                states: 81
                compiled_words: 10,290
                forced_words: 33,886
                forcing: inputs (exponential complete automaton)
              memory 1/100:
                colors: 3
                states: 101
                compiled_words: 10,290
                forced_words: 45,706
                forcing: inputs (exponential complete automaton)
              memory 1/1000:
                colors: 3
                states: 1,001
                compiled_words: 10,290
                forced_words: 3,061,990
                forcing: inputs (exponential complete automaton)
              memory 2/10:
                colors: 4
                states: 11
                compiled_words: 10,308
                forced_words: 11,698
                forcing: inputs (exponential complete automaton)
              memory 2/20:
                colors: 4
                states: 21
                compiled_words: 10,308
                forced_words: 14,063
                forcing: inputs (exponential complete automaton)
              memory 2/40:
                colors: 4
                states: 41
                compiled_words: 10,308
                forced_words: 22,693
                forcing: inputs (exponential complete automaton)
              memory 2/80:
                colors: 4
                states: 81
                compiled_words: 10,308
                forced_words: 55,553
                forcing: inputs (exponential complete automaton)
              memory 2/100:
                colors: 4
                states: 101
                compiled_words: 10,308
                forced_words: 79,783
                forcing: inputs (exponential complete automaton)
              memory 2/1000:
                colors: 4
                states: 1,001
                compiled_words: 10,308
                forced_words: 6,552,517
                forcing: inputs (exponential complete automaton)
              expression IDs/broad/16/1024:
                colors: 9
                states: 1,033
                compiled_words: 11,053
                forced_words: 98,863
                forcing: full
              expression IDs/broad/4096/16384:
                colors: 17
                states: 16,401
                compiled_words: 350,486
                forced_words: 2,449,128
                forcing: full
              expression IDs/broad/65536/262144:
                colors: 21
                states: 262,165
                compiled_words: 5,996,822
                forced_words: 42,567,884
                forcing: full
              expression IDs/narrow/1024:
                colors: 2
                states: 1,027
                compiled_words: 10,492
                forced_words: 80,655
                forcing: full
              expression IDs/narrow/1000000:
                colors: 2
                states: 1,000,003
                compiled_words: 10,000,252
                forced_words: 78,024,559
                forcing: full
              |}]
          ;;
        end
        in
        ()
    ;;
  end)
;;
