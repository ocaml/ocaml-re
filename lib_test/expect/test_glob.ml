open Import

let glob ?match_backslashes ?expand_braces ?anchored ?pathname ?period re s =
  let re =
    Re.Glob.glob ?match_backslashes ?expand_braces ?anchored ?pathname ?period re
    |> Re.compile
  in
  Format.printf "%b@." (Re.execp re s)
;;

let%expect_test "glob" =
  glob "foo*" "foobar";
  [%expect {| true |}];
  glob "fo?bar" "fobar";
  [%expect {| false |}];
  glob "fo?bar" "foobar";
  [%expect {| true |}];
  glob "fo?bar" "foo0bar";
  [%expect {| false |}];
  glob "?oobar" "foobar";
  [%expect {| true |}];
  glob "*bar" "foobar";
  [%expect {| true |}];
  glob "\\*bar" "foobar";
  [%expect {| false |}];
  glob "\\*bar" "*bar";
  [%expect {| true |}];
  glob "[ab]foo" "afoo";
  [%expect {| true |}];
  glob "[ab]foo" "bfoo";
  [%expect {| true |}];
  glob "[ab]foo" "cfoo";
  [%expect {| false |}];
  glob "c[ab]foo" "cabfoo";
  [%expect {| false |}];
  glob ".foo" ".foo";
  [%expect {| true |}];
  glob ".foo" "afoo";
  [%expect {| false |}];
  glob "*[.]foo" "a.foo";
  [%expect {| true |}];
  glob "*[.]foo" "ba.foo";
  [%expect {| true |}];
  glob "*.foo" ".foo";
  [%expect {| false |}];
  glob "*[.]foo" ".foo";
  [%expect {| false |}];
  glob ~anchored:true "*/foo" "/foo";
  [%expect {| true |}];
  glob ~anchored:true "foo/*" "foo/";
  [%expect {| true |}];
  glob "/[^f]" "/foo";
  [%expect {| false |}];
  glob "/[^f]" "/bar";
  [%expect {| true |}];
  glob ~anchored:true "/[^f]" "/bar";
  [%expect {| false |}];
  glob ~anchored:true "*" ".bar";
  [%expect {| false |}];
  glob "foo[.]bar" "foo.bar";
  [%expect {| true |}];
  glob "[.]foo" ".foo";
  [%expect {| false |}];
  glob "foo[/]bar" "foo/bar";
  [%expect {| false |}];
  glob ~anchored:true "*bar" "foobar";
  [%expect {| true |}];
  glob "foo" "foobar";
  [%expect {| true |}];
  glob "bar" "foobar";
  [%expect {| true |}];
  glob ~anchored:true "foo" "foobar";
  [%expect {| false |}];
  glob ~anchored:true "bar" "foobar";
  [%expect {| false |}];
  glob "{foo,bar}bar" "foobar";
  [%expect {| false |}];
  glob "{foo,bar}bar" "{foo,bar}bar";
  [%expect {| true |}];
  glob "foo?bar" "foo/bar";
  [%expect {| false |}];
  let pathname = true in
  let period = true in
  glob ~pathname ~period "?oobar" ".oobar";
  [%expect {| false |}];
  glob ~pathname ~period "?oobar" "/oobar";
  [%expect {| false |}];
  glob ~pathname ~period "f?obar" "f/obar";
  [%expect {| false |}];
  glob ~pathname ~period "f?obar" "f.obar";
  [%expect {| true |}];
  glob ~pathname ~period "f*.bar" "f.bar";
  [%expect {| true |}];
  glob ~pathname ~period "f?.bar" "fo.bar";
  [%expect {| true |}];
  glob ~pathname ~period "/.bar" "/.bar";
  [%expect {| true |}];
  glob ~pathname ~period "*.bar" ".bar";
  [%expect {| false |}];
  glob ~pathname ~period "?" ".";
  [%expect {| false |}];
  glob ~pathname ~period "/*bar" "/.bar";
  [%expect {| false |}];
  glob "?oobar" ".oobar";
  [%expect {| false |}];
  glob "?oobar" "/oobar";
  [%expect {| false |}];
  let pathname = true in
  let period = false in
  glob ~pathname ~period "?oobar" "/oobar";
  [%expect {| false |}];
  glob ~pathname ~period "?oobar" ".oobar";
  [%expect {| true |}];
  glob ~pathname ~period "f?obar" "f/obar";
  [%expect {| false |}];
  glob ~pathname ~period "f?obar" "f.obar";
  [%expect {| true |}];
  let pathname = false in
  let period = false in
  glob ~pathname ~period "?oobar" ".oobar";
  [%expect {| true |}];
  glob ~pathname ~period "?oobar" "/oobar";
  [%expect {| true |}];
  glob ~expand_braces:true "{foo,far}bar" "foobar";
  [%expect {| true |}];
  glob ~expand_braces:true "{foo,far}bar" "farbar";
  [%expect {| true |}];
  glob ~expand_braces:true "{foo,far}bar" "{foo,far}bar";
  [%expect {| false |}]
;;

let%expect_test "double asterisk" =
  let glob = glob ~anchored:true in
  glob "**" "foobar";
  [%expect {| true |}];
  glob "**" "foo/bar";
  [%expect {| true |}];
  glob "**/bar" "foo/bar";
  [%expect {| true |}];
  glob "**/bar" "foo/far/bar";
  [%expect {| true |}];
  glob "foo/**" "foo";
  [%expect {| false |}];
  glob "foo/**" "foo/bar";
  [%expect {| true |}];
  glob "foo/**" "foo/far/bar";
  [%expect {| true |}];
  glob "foo/**/bar" "foo/far/bar";
  [%expect {| true |}];
  glob "foo/**/bar" "foo/far/oof/bar";
  [%expect {| true |}];
  glob "foo/**bar" "foo/far/oofbar";
  [%expect {| true |}];
  glob "foo/**bar" "foo/bar";
  [%expect {| true |}];
  glob "foo/**bar" "foo/foobar";
  [%expect {| true |}];
  glob "/**" "//foo";
  [%expect {| true |}];
  glob "/**" "/";
  [%expect {| true |}];
  glob "/**" "/x";
  [%expect {| true |}];
  glob "**" "foo//bar";
  [%expect {| true |}];
  glob "foo/bar/**/*.ml" "foo/bar/baz/foobar.ml";
  [%expect {| true |}];
  glob "foo/bar/**/*.ml" "foo/bar/foobar.ml";
  [%expect {| true |}];
  glob "foo/**/bar/**/baz" "foo/bar/baz";
  [%expect {| true |}];
  glob "foo/**/bar/**/baz" "foo/bar/x/y/z/baz";
  [%expect {| true |}];
  glob "foo/**/bar/**/baz" "foo/x/y/z/bar/baz";
  [%expect {| true |}];
  glob "foo/**/bar/**/baz" "foo/bar/x/bar/x/baz";
  [%expect {| true |}];
  glob "foo/**/bar/**/baz" "foo/bar/../x/baz";
  [%expect {| false |}];
  glob "foo/**/bar/**/baz" "foo/bar/./x/baz";
  [%expect {| false |}];
  ((* Interaction with [~period] *)
   let glob = glob ~period:true in
   glob "**" ".foobar";
   [%expect {| false |}];
   glob "**" ".foo/bar";
   [%expect {| false |}];
   glob "foo/**" "foo/.bar";
   [%expect {| false |}];
   glob "**" "foo/.bar/bat";
   [%expect {| false |}];
   glob "foo/**/bat" "foo/.bar/bat";
   [%expect {| false |}];
   glob "/**/bat" "/foo/.bar/bat";
   [%expect {| false |}];
   glob "/**/bat" "/.bar/bat";
   [%expect {| false |}];
   glob "/**bat" "/bar/.bat";
   [%expect {| false |}];
   glob ".**" ".foobar";
   [%expect {| true |}];
   glob ".**" ".foo/bar";
   [%expect {| true |}];
   glob "foo/.**" "foo/.bar";
   [%expect {| true |}]);
  let glob = glob ~period:false in
  glob "**" ".foobar";
  [%expect {| true |}];
  glob "**" ".foo/bar";
  [%expect {| true |}];
  glob "foo/**" "foo/.bar";
  [%expect {| true |}];
  glob "**" "foo/.bar/bat";
  [%expect {| true |}];
  glob "foo/**/bat" "foo/.bar/bat";
  [%expect {| true |}];
  glob "/**/bat" "/foo/.bar/bat";
  [%expect {| true |}];
  glob "/**/bat" "/.bar/bat";
  [%expect {| true |}];
  glob "/**bat" "/bar/.bat";
  [%expect {| true |}]
;;

let%expect_test "backslash handling" =
  let anchored = true in
  let glob = glob ~anchored in
  (let glob = glob ~match_backslashes:false in
   glob "a/b/c" "a\\b/c";
   [%expect {| false |}];
   glob "a\\b" "ab";
   [%expect {| true |}];
   glob "a/*.ml" "a/b\\c.ml";
   [%expect {| true |}];
   glob "a/b/*.ml" "a\\b\\c.ml";
   [%expect {| false |}];
   glob "/" "\\";
   [%expect {| false |}];
   glob "/?" "\\a";
   [%expect {| false |}];
   glob "a/**.ml" "a\\c\\.b.ml";
   [%expect {| true |}]);
  let glob = glob ~match_backslashes:true in
  glob "a/b/c" "a\\b/c";
  [%expect {| true |}];
  glob "a\\b" "ab";
  [%expect {| true |}];
  glob "a/*.ml" "a/b\\c.ml";
  [%expect {| false |}];
  glob "a/b/*.ml" "a\\b\\c.ml";
  [%expect {| true |}];
  glob "/" "\\";
  [%expect {| true |}];
  glob "/?" "\\a";
  [%expect {| true |}];
  glob "a/**.ml" "a\\c\\.b.ml";
  [%expect {| false |}]
;;
