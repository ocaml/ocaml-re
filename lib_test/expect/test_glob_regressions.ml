let%expect_test "globstars must not bypass leading-period restrictions" =
  Test_glob.glob ~anchored:true "**/*" ".hidden";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "**/*" "a/.hidden";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "**?" ".";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "**.txt" ".txt";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "a**?x" "a/.x";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "a**.x" "a/.x";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "a**?" "a/";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true "**/.hidden" ".hidden";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true "a**.x" "a.x";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true "**?" "a";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true "**/*" "a/file";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true "**/.*" "a/.hidden";
  [%expect {| true |}];
  let pattern = String.concat "" (List.init 30 (fun _ -> "a**")) in
  Test_glob.glob ~anchored:true pattern (String.make 30 'a');
  [%expect {| true |}]
;;

let%expect_test "leading-period restrictions also apply without pathname matching" =
  Test_glob.glob ~anchored:true ~pathname:false "*.txt" ".txt";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true ~pathname:false "*[.]txt" ".txt";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true ~pathname:false "*?txt" ".txt";
  [%expect {| false |}];
  Test_glob.glob ~anchored:true ~pathname:false "*.txt" "a/.txt";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true ~pathname:false "*.txt" "a.txt";
  [%expect {| true |}];
  Test_glob.glob ~anchored:true ~pathname:false ".*" ".txt";
  [%expect {| true |}]
;;

let%expect_test "brace expansion preserves braces without alternatives" =
  List.iter
    (fun (pattern, input) ->
       Test_glob.glob ~anchored:true ~expand_braces:true pattern input)
    [ "{foo}", "{foo}"
    ; "{foo}", "foo"
    ; "{{a,b}}", "{a}"
    ; "{{a,b}}", "{b}"
    ; "{a,b}{c,d}", "bd"
    ; "{}", "{}"
    ; "{,}", ""
    ];
  [%expect
    {|
    true
    false
    true
    true
    true
    true
    true
    |}]
;;
