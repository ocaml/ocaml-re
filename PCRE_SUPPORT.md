# PCRE pattern compatibility audit

## Scope and references

`Re.Pcre.re` delegates to `Re.Perl.re` in `lib/perl.ml`; it is not a binding
of PCRE. This audit compares that parser and the byte-oriented `Re` AST/matcher
with the default **8-bit, non-UTF PCRE2 pattern language**, using LF newlines.
PCRE is an implementation-defined language, not a separate formal standard.
The references are the upstream manuals:

- [pcre2pattern](https://www.pcre.org/current/doc/html/pcre2pattern.html)
  (25 October 2025 revision).
- [pcre2syntax](https://www.pcre.org/current/doc/html/pcre2syntax.html)
  (14 October 2025 revision).

The inventory is by feature family, not an exhaustive enumeration of every
combination of constructs. `lib_test/expect/test_pcre_conformance.ml` records
representative missing syntax **and accepted patterns with wrong semantics**.
Each case retains its expected PCRE result; snapshots show actual disagreements.
The tests run without a PCRE dependency and exercise both public parsers.

Unicode/UTF, properties, graphemes, script runs, Unicode casing, Unicode names,
and backreferences are explicitly out of scope. Backreferences will not be
implemented. Bytes 0x80–0xff are still bytes, not UTF sequences. EBCDIC is also
outside the target. `\g<...>` and `\g'...'` are subroutine calls, *not*
backreferences, and are included in the inventory.

## Existing support

Literals, escaped punctuation, concatenation, ordered alternatives, ordinary
and noncapturing groups, `(?<name>...)`, `(?#...)`, dot, positive/negative
classes, ranges, POSIX named classes (including negation), greedy/lazy repeats,
`\d\D\s\S\w\W`, `\b\B\A\Z\z\G`, and external caseless, multiline,
dotall and anchored flags are present. Perl also exposes ungreedy and
Dollar_endonly. Braced hex/octal decoding and octal byte-range validation are
already present. Several supported features still have compatibility defects
listed below.

## Gaps at the audit baseline

| Area | Missing or incorrect behavior | Feasibility |
| --- | --- | --- |
| Single-byte escapes | `\a`, `\cX`; one-digit hex; short octal including `\0`; escapes inside classes/ranges | Parser-only |
| Braced numeric escapes | Leading zeroes in hex; whitespace inside braces; hex/octal escapes inside classes | Parser-only; retain byte-range and overflow rejection |
| Character types | `\h\H\v\V`, `\N`, `\C`; `\R` | All but `\R` straightforward; `\R` requires atomic CRLF handling, not a plain alternation |
| Class complement | `\W` inside a class is implemented as `\w` | Parser-only bug |
| Character tables | `\w`, word boundaries, several POSIX classes and caseless matching use Latin-1 tables, unlike PCRE's default C-locale tables | Compatibility decision; keep core character tables unchanged in this pass |
| Quoting | `\Q` to end of pattern; trailing backslash; stray `\E`; quotes in classes; overlapping `\\E`; quantifiers should apply only to the last quoted character, not the whole quoted run | Lexer/parser work |
| Inline comments | `(?#...)` is parsed as an empty atom, so a following quantifier does not bind to the preceding real atom | Lexer/parser work, like quoting |
| Quantifiers | `{,m}` (PCRE2 10.43+), whitespace (10.44+), literal malformed/non-quantifier braces, quoted braces; assertions incorrectly quantifiable | Parser-only, with careful tokenization and resource limits |
| Empty repetitions | `(a){0}(b)` loses the first capture's slot | AST/compiler change, not merely parser syntax |
| Anchors | Default `$` and Dollar_endonly are reversed; multiline `^` matches after a final newline | Dollar is parser-only; final-newline circumflex needs a suitable assertion |
| Boundary aliases | `[[:<:]]`, `[[:>:]]` | Existing beginning/end-of-word AST nodes |
| Match reset | `\K` | Requires group-0 start tracking distinct from consumed prefix |
| Named groups | `(?'name'...)`, `(?P<name>...)`; duplicate names accepted without `(?J)`; no PCRE name length limit | Aliases are parser-only; validation/options remain |
| Branch reset | `(?|...)` | Capture numbering needs AST/compiler support |
| Internal options | `(?imnsx)`, `(?xx)`, `(?U)`, `(?J)`, unset/reset/empty/scoped forms, ASCII restriction options | Feasible parser state; must restore on group exit and propagate across alternatives |
| Extended classes | Perl `(?[...])` with union, intersection, difference, xor, complement and grouping (10.45+) | Existing character-set AST operations can express these |
| Class validation | Reversed ranges, set endpoints, POSIX collating/equivalence elements accepted or diagnosed differently from PCRE | Parser validation; Perl has some deliberately more permissive syntax |
| Lookaround | Positive/negative ahead/behind, bounded variable lookbehind, long/short alphabetic aliases; non-atomic assertions | No general assertion node in current AST; do not fake them by consuming characters |
| Atomicity | `(?>...)`, `(*atomic:...)`, all possessive quantifiers | No atomic node; greedy/longest matching is **not** equivalent |
| Conditions | Capture participation (absolute/relative/named), assertion, recursion, DEFINE and VERSION conditions | Requires state/AST work; VERSION also requires an explicit compatibility policy |
| Subroutines | Numeric/relative/named/Python/Oniguruma calls, forward calls, returning captures (10.47+) | Acyclic expansion conceivable; capture restoration/numbering complicate it |
| Recursion | Whole-pattern and group recursion, recursion conditions | General recursion can describe nonregular languages; outside this automaton's capabilities |
| Scan substring | `(*scan_substring:...)`, `(*scs:...)` | Needs access to captured subject regions during matching |
| Verbs | FAIL/F, ACCEPT, MARK/:name, COMMIT, PRUNE, SKIP (including named), THEN | Only unlabelled FAIL/F readily lowers to the empty language; others need control-flow/mark semantics |
| Callouts | `(?C)`, numeric/string callouts, conditional callouts | No callback API or execution model; silently ignoring is not full support |
| Newline settings | `(*CR)`, `(*LF)`, `(*CRLF)`, `(*ANYCRLF)`, `(*ANY)`, `(*NUL)`; BSR_ANYCRLF/BSR_UNICODE | LF is currently hardwired; CRLF needs multi-byte boundary semantics |
| Leading directives | NOTEMPTY/NOTEMPTY_ATSTART, optimization/JIT controls, LIMIT_MATCH/DEPTH/RECURSION/HEAP | No matching PCRE execution/resource-limit model; do not silently claim these guarantees |

The snapshot tests intentionally pass while demonstrating missing support. A
successful parse alone is not counted as implementation: matches, nonmatches,
and malformed-pattern rejection are checked. Capture bookkeeping is separately
probed. Tests distinguish Parse_error from Not_supported; existing diagnostics
do not reliably distinguish valid-but-unimplemented PCRE syntax from invalid
syntax.

## API-only PCRE features

The PCRE C API is not this library's API. Compile/match contexts, custom locale
tables, JIT, callouts, configurable resource limits, alternative/extra compile
modes (ALT_BSUX, ALT_EXTENDED_CLASS, ALLOW_EMPTY_CLASS, ALT_CIRCUMFLEX, etc.),
NOTBOL/NOTEOL, NOTEMPTY, partial-mode selection, and the PCRE substitution-string
language have no direct equivalent in `Re.Pcre`. Some functionality exists via
`Re` combinators or other functions, but that is not PCRE syntax support.
`Re.Pcre.substitute` takes an OCaml callback, not a PCRE replacement template.
This audit's executable inventory targets patterns, not full API parity or
all behaviors of the pcre-ocaml compatibility wrappers.

## Implementation policy

Prefer byte-only parser changes expressible exactly by the existing AST. Keep
unsupported constructs rejected rather than translating them into a superficially
similar expression with different matching/capture behavior. Follow-up work must
retain negative and context-sensitive cases, not only positive matching examples.
