Unreleased
----------

* Fix [Re.Glob] with `pathname` disabled allowing wildcards to match a leading
  period; leading dots must still be matched explicitly (#708).

* Fix [Re.witness] raising [Assert_failure] for languages containing empty
  alternatives; empty languages now raise [Invalid_argument] (#685).

* Skip inline `(?#...)` comments when attaching quantifiers in [Re.Perl] and
  [Re.Pcre], matching PCRE: `a(?#note)*` is `a*`, and a quantifier with no
  remaining operand is a parse error (#702).

* Treat `\Q...\E` quoting in [Re.Perl] and [Re.Pcre] as lexical, matching
  PCRE: a quantifier after `\E` applies to the last quoted byte, an empty
  quote does not satisfy a quantifier, unterminated quotes extend to the end
  of the pattern, and quoting works inside character classes (#688).

* Fix [Re.Glob] with `expand_braces:true` treating `{foo}` as `foo`; braces
  without alternatives are now literal (#686).

* Fix [Re.Stream.finalize] not treating a newline at the end of a finalized
  window as the final newline (#694).

* Validate core execution bounds without overflow: reject positions past the
  input and avoid overflow when checking lengths (#693).

* Fix [Re.Stream] entry points silently accepting invalid `(pos, len)`
  windows; they now raise [Invalid_argument] (#692).

* Fix completed [Re.Stream.Group] matches being corrupted when the underlying
  stream is reused (#687).

* Support byte-mode `\h`, `\H`, `\v`, `\V`, `\C`, and `\N` in
  `Re.Perl` and `Re.Pcre`. Fix `\W` inside character classes (#682).

* Fix [Re.Str] treating descending character ranges such as `[z-a]` as a
  reversed range; they now denote the empty set, matching [Str] (#683).

* Extend byte escapes in `Re.Perl` and `Re.Pcre`: short octal and hex
  forms, leading zeroes and brace whitespace, and escapes inside character
  classes. Add `\a` and `\cX`; reject malformed and out-of-range values
  (#679).

* Fix corrupted captures in [Re.Stream.Group] when feeding or finalizing
  input windows with nonzero source offsets (#664).

* Fix crashes when feeding or finalizing [Re.Stream] and [Re.Stream.Group]
  after reaching a terminal match state (#665).

* Allow newlines in negated [Re.Posix] character classes unless the [Newline]
  option is enabled (#662).

* Allow [Re.Str.group_beginning] and [Re.Str.group_end] to access capture
  groups above nine, matching [Str] (#661).

* Raise [Re.Glob.Parse_error] for dangling escapes during brace expansion,
  rather than leaking a substring bounds exception (#660).

* Fix `$` in [Re.Perl] and [Re.Pcre] to match before a final newline by
  default. `Dollar_endonly` now requires the end of input; multiline behavior
  is unchanged (#671).

* Support `(?'name'...)` and `(?P<name>...)` capture-group aliases and
  `[[:<:]]` / `[[:>:]]` word-boundary aliases in [Re.Perl] and [Re.Pcre] (#672).

* Reject integer-overflowing repetition counts in [Re.Perl], [Re.Posix], and
  [Re.Pcre] instead of accepting wrapped bounds (#663).

* Fix [Re.Str] replacement references to unmatched groups to raise [Failure],
  as [Str] does, instead of substituting an empty string (#646).

* Fix incorrect captured text and assertion failures in [Re.Stream.Group]
  when discarding unmatched input prefixes (#645).

* Fix character-set subtraction when one interval spans several disjoint
  intervals in the source set (#632).

* Fix braced octal escapes in [Re.Perl] and [Re.Pcre] to use positional
  decoding and reject empty or out-of-byte-range values before overflow (#634).

* Fix two-digit braced hexadecimal escapes in [Re.Perl] and [Re.Pcre]
  being decoded in reverse order (#633).

* Fix [Re.Pcre.split] dropping or not dropping leading and trailing
  delimiters consistently with [Pcre.split] (#602, fixes #590).

* Fix [Re.Glob] issue where `a/**b` matched `ab` (#611)

* Change [Re.Glob] so `**/a` matches `a`, as that's least surprising
  (git, bash, hg and rg all agree on this). The only change is at start of
  glob, as `a/**/b` already matched `a/b`. Should you want to match at
  least one directory deep, you can use `*/**/a`. (#611, fixes #171)

* Fix issue where the requested match semantics (e.g. requesting the
  longest match for a subexpression) was not respected in some niche
  scenarios (#610).

* Fix the documentation of [Re.exec\_partial] and
  [Re.exec\_partial\_detailed], and their implementations to follow
  the documentation. (#601, #604, fixes #600)

1.14.0 (16-Sep-2025)
--------------------

* Thread-safety with OCaml 5 (#574).

* Introduce [Re.Pcre.get_named_substring_opt]. A non raising version of
  [Re.Pcre.get_named_substring] (#525)

* Introduce parsing functions in `Re.{Perl,Pcre,Emacs,Glob}` that return a
  result instead of raising. (#542)

* Introduce experimental streaming API `Re.Stream`. (#456)

* Make [Re.Str] functions tail recursive (#539)

* Fix [Re.Pcre.split]. Regression introduced in 1.12 and a previous bug with
  [Re.Pcre.split] (#538).

* Avoid parsing unnecessary patterns supported only by `Re.Emacs` in `Re.Str`
  (#563)

1.13.1 (30-Sep-2024)
--------------------

* Fix re on jsoo (#150)

1.13.0 (30-Sep-2024)
--------------------

* Add non raising versions of all [Re.Group] functions (#414, fixes #150)

* Add support for hex and octal of the form: `\o{...}` and `\x{...}` (#403)

* Add support for octal characters using `\0dd` and `\ddd` (#402)

* Add support for `\Q...\E` quoted expressions in Pcre and Perl syntax (#401)

* Re.execp and related function raise [Invalid_argument "$function"] when [pos]
  or [len] arguments are out of bounds. In 1.12.0, a regerssion was introduced
  that raised [Invalid_argument _] from [String.get].

1.12.0 (29-Aug-2024)
--------------------

* Add `Re.split_delim` (#233)
* Fix handling of empty matches in splitting and substitution functions (#233)
* Add support for character classes in `Re.Posix` (#263)

1.11.0 (19-Aug-2023)
--------------------

* Add `Re.group_count` to get the number of groups in a compiled regex (#218)
* Add `Re.exec_partial_detailed` to allow resuming searches from partial inputs
  (#219)
* Re-export `Re.Perl`'s `Parse_error` and `Not_supported` exceptions
  in Pcre (#222)
* Add support for `DOTALL` flag in `Re.Pcre.regexp` (#225)
* Add support for named groups (#223)
* Add support for some control characters in `Re.Perl` (#227)

1.10.4 (27-Apr-2022)
--------------------

* Improve handling of word boundaries (#179)

1.10.3 (13-Sep-2021)
--------------------

* Glob: change optional argument `?backslash_escapes` to `?match_backslashes`.
  The interpretation of backslashes in the glob pattern remains unchanged with
  the new option, but forward slashes match backslashes when activated (#199)

1.10.2 (09-Sep-2021)
--------------------

* Fix missing aliases introduced in 1.10.1

1.10.1 (08-Sep-2021)
--------------------

* Glob: add optional argument `?backslash_escapes` to control interpretation of
  backslashes (useful under Windows) (#197, #198)

* Restore accidentally deleted `*_seq` deprecated aliases.

1.10.0 (25-Aug-2021)
--------------------

* Add the `[:alpha:]` character class in `Re.Perl` (#169)
* Double asterisk (`**`) in `Re.Glob` (#172)
  Like `*` but also match `/` characters when `pathname` is set.
* Double asterisk should match 0 or more directories unless in trailing
  position. (#192, fixes #185)

1.9.0 (05-Apr-2019)
-------------------

* Fix regression in `Re.exec_partial` (#164)
* Mov gen related functions to `Re.Gen` and deprecate the old names (#167)
* Introduce `Re.View` that exposes the internal representation (#163)

1.8.0 (04-Aug-2018)
-------------------

* Fix index-out-of-bounds exception in Re.Perl.re (#160)
* Add seq based iterators (#170)

1.7.3 (05-Mar-2018)
-------------------

* Remove dependency on bytes package (#155)

1.7.2 (01-Mar-2018)
-------------------

* Deprecate all Re_* modules. Re_x is now available as Re.X
* Deprecate all re.x sub libraries. Those are all available as Re.X
* Make all function in Re.Str tail recursive.

1.7.1 (19-Oct-2016)
-------------------

* Fix Re_str.global_replace (#132)

1.7.0 (18-Sep-2016)
-------------------

* Fix stack overflow in Re_str.full_split
* Use correct exceptions in Re_str group functions
* Add experimental Re.witness
* Add experimental Re.Group.nb_groups

1.6.1 (20-Jun-2016)
-------------------

* Fix Re.pp (#101)
* Add Re.Group.pp (#102)

1.6.0 (30-May-2016)
-------------------

* Add Re.pp and Re.pp_re (#55)
* Fix ocamldoc syntax (#87)

1.5.0 (04-Jan-2016)
-------------------

* Add Re.exec_opt. Like exec but doesn't raise
* Add Group module. Old group accessors are deprecated.
* Add Mark module
* Improve docs of Re.repn
* Improve docs of Re_pcre
* Fix doc of Re_pcre.match
* Consolidate variants of Re.glob that takes options to modify its behavior
  (?period, ?expand_braces). Old variants are deprecated.
* New option ?pathname added for Re_glob.glob. Controls how the `/` character
  is matched

1.4.1 (06-Jun-2015)
-------------------

* Fix 4.00.1 compatibilty with tests.

1.4.0 (12-May-2015)
-------------------

* Add Re.{mark,marked,mark_set}. Regexps can now be "marked" to query post
  execution if they matched.

1.3.2 (14-Apr-2015)
-------------------

* Fix replacing 0 length matches (#55)

1.3.1 (13-Mar-2015)
-------------------

* Rename {Cset, Automata} to {Re_cset, Re_automata}

1.3.0 (02-Feb-2015)
-------------------

* Add Re.split{,_gen,_token,_full,_full_gen}
* Add Re.replace{,_string}
* Add Re.all{,_gen}
* Add posix classes of the form [:xxx:]
* Add complement suport for posix classes
* Add Multiline and anchored flag to Re_pcre
* Add Re_pcre.full_split

1.2.2 (05-May-2014)
-------------------

* Add a Re.whole_string convenience function to only match whole strings
* Add a ?anchored parameter to functions in Re_glob to specify whole
  string matching
* Document Re_glob module
* Fix compilation of submatches occurring inside a Kleen star
* Fix word boundary matching
* Fix definition of Re.xdigit
* Fix Re.exec_partial function
* Fix compilation of patterns of the shape r1r2|r1r3
* Fixed compilation of re.cmxs (Vincent Bernardoff)
* Improved matching of anchored regular expressions: stop as soon as
  we know there cannot possibly be any match.
* Updated to OASIS 0.4.x (Vincent Bernardoff)
* Add the linking exception to the license

1.2.1 (07-Apr-2013)
-------------------

* Correct OASIS metadata (Christophe Troestler).
* Fix typo in Invalid_arg error message (Jeremy Yallop).

1.2.0 (15-Jan-2012)
-------------------

* Rename Pcre module to `Re_pcre` to make it more suitable for
  upstream packaging (it currently conflicts with the `Pcre` package).
  (Mehdi Dogguy).

1.1.0 (05-Sep-2012)
-------------------

* Add a basic Pcre wrapper around Re_perl for porting applications using that
  API (Thomas Gazagnaire).

1.0.0 (01-Aug-2012)
-------------------

* Initial public release.
