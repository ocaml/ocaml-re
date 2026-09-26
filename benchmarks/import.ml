(* This library is preprocessed with ppx_expect, whose runtime depends on the
   released [re]. Use the mangled copy of the library (as lib_test/expect does)
   so the inline-test runner does not link two modules named [Re__]. *)
module Re = Re_private.Re
