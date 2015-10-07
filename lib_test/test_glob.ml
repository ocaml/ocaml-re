open Re_glob

let re_match ?pos ?len re s =
  Re.execp ?pos ?len (Re.compile re) s
;;

let re_mismatch ?pos ?len re s = not (re_match ?pos ?len re s)

let _ =
  assert (re_match    (glob "foo*")   "foobar" );
  assert (re_mismatch (glob "fo?bar") "fobar"  );
  assert (re_match    (glob "fo?bar") "foobar" );
  assert (re_mismatch (glob "fo?bar") "foo0bar");
  assert (re_match    (glob "?oobar") "foobar" );
  assert (re_match    (glob "*bar")   "foobar" );
  assert (re_mismatch (glob "\\*bar") "foobar" );
  assert (re_match    (glob "\\*bar") "*bar"   );

  assert (re_match    (glob "[ab]foo")  "afoo"  );
  assert (re_match    (glob "[ab]foo")  "bfoo"  );
  assert (re_mismatch (glob "[ab]foo")  "cfoo"  );
  assert (re_mismatch (glob "c[ab]foo") "cabfoo");

  assert (re_match    (glob ".foo"   ) ".foo"  );
  assert (re_mismatch (glob ".foo"   ) "afoo"  );
  assert (re_match    (glob "*[.]foo") "a.foo" );
  assert (re_match    (glob "*[.]foo") "ba.foo");
  assert (re_mismatch (glob "*.foo"  ) ".foo"  );
  assert (re_mismatch (glob "*[.]foo") ".foo"  );

  assert (re_match    (glob ~anchored:true "*/foo") "/foo");
  assert (re_match    (glob ~anchored:true "foo/*") "foo/");

  assert (re_mismatch (glob ~anchored:true "*") ".bar");

  assert (re_match    (glob "foo[.]bar") "foo.bar");
  assert (re_mismatch (glob "[.]foo"   ) ".foo"   );
  assert (re_mismatch (glob "foo[/]bar") "foo/bar");

  assert (re_match    (glob ~anchored:true "*bar") "foobar");

  assert (re_match    (glob                "foo") "foobar");
  assert (re_match    (glob                "bar") "foobar");
  assert (re_mismatch (glob ~anchored:true "foo") "foobar");
  assert (re_mismatch (glob ~anchored:true "bar") "foobar");

  assert (re_mismatch (glob "{foo,bar}bar") "foobar"      );
  assert (re_match    (glob "{foo,bar}bar") "{foo,bar}bar");
  assert (re_mismatch (glob "foo?bar"     ) "foo/bar"     );

  assert (re_mismatch (glob' true  "?oobar") ".oobar");
  assert (re_mismatch (glob        "?oobar") ".oobar");
  assert (re_match    (glob' false "?oobar") ".oobar");

  assert (re_match    (globx "{foo,far}bar") "foobar"      );
  assert (re_match    (globx "{foo,far}bar") "farbar"      );
  assert (re_mismatch (globx "{foo,far}bar") "{foo,far}bar");
