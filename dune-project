(lang dune 3.15)

(name re)

(generate_opam_files true)

(implicit_transitive_deps false)

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(maintainers "Rudi Grinberg <me@rgrinberg.com>")
(authors
  "Jerome Vouillon"
  "Thomas Gazagnaire"
  "Anil Madhavapeddy"
  "Rudi Grinberg"
  "Gabriel Radanne")

(source (github ocaml/ocaml-re))

(package
 (name re)
 (synopsis "RE is a regular expression library for OCaml")
 (depends
  (ocaml (>= 4.12.0))
  (ppx_expect :with-test)
  (ounit2 :with-test)
  (js_of_ocaml :with-test))
 (description "
Pure OCaml regular expressions with:
* Perl-style regular expressions (module Re.Perl)
* Posix extended regular expressions (module Re.Posix)
* Emacs-style regular expressions (module Re.Emacs)
* Shell-style file globbing (module Re.Glob)
* Compatibility layer for OCaml's built-in Str module (module Re.Str)
"))
