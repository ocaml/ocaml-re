(* Workhorse patterns that show up repeatedly in real OCaml code (harvested
   from GitHub call sites and local checkouts of dune, opam, xapi, liquidsoap,
   magic-trace, ocaml-lsp). They are deliberately the unglamorous idioms --
   whitespace, numbers, key/value lines, quoted strings -- that the
   validation-oriented patterns in [Common_patterns] do not cover. Samples only
   establish polarity; the workloads care about colors, states and forcing. *)

open Pattern

(* Whitespace and record structure. *)

let trim =
  perl
    "idiom/whitespace/trim"
    {|^[ \t]+|[ \t]+$|}
    [ "  hello"; "hello "; " hello world " ]
    [ "hello"; "hello world" ]
;;

let whitespace_run =
  (* The single most repeated pattern in real code. *)
  perl "idiom/whitespace/run" {|\s+|} [ "a b"; "a\tb"; "a  b" ] [ "ab" ]
;;

let tab_separated =
  perl
    "idiom/list/tab-separated"
    {|^([^\t]*)\t([^\t]*)\t([^\t]*)$|}
    [ "a\tb\tc"; "1\t\t3" ]
    [ "a,b,c"; "a\tb" ]
;;

let separator_run =
  perl "idiom/list/separator-run" {|[;,][ \t]*|} [ "a, b"; "a;b"; "a,b" ] [ "ab" ]
;;

let crlf = perl "idiom/line/crlf" {|\r?\n|} [ "a\r\nb"; "a\nb" ] [ "ab" ]
let blank_line = perl "idiom/line/blank" {|(?m)^[ \t]*$|} [ "\n"; "a\n  \nb" ] [ "a\nb" ]

let hash_comment =
  perl
    "idiom/comment/hash"
    {|(?m)^[ \t]*#[^\n]*|}
    [ "# comment"; "a\n  # note\n"; "#!/bin/sh" ]
    [ "a # not at line start" ]
;;

(* Numbers. *)

let integer =
  perl "idiom/number/integer" {|-?\d+|} [ "42"; "-7"; "v1.2-3" ] [ "abc"; "--" ]
;;

let float =
  perl
    "idiom/number/float"
    {|([0-9]+)(\.([0-9]*))?|}
    [ "3.14"; "2."; "42" ]
    [ "xyz"; ".." ]
;;

let hex =
  perl
    "idiom/number/hex"
    {|0x[0-9a-fA-F]+|}
    [ "0xdeadBEEF"; "addr 0x1f" ]
    [ "0xZZ"; "1234" ]
;;

(* Configuration lines. *)

let key_value =
  perl
    "idiom/config/key-value"
    {|([^= \t]+)[ \t]*=[ \t]*(.*)|}
    [ "name = value"; "a=1"; "key=\tval" ]
    [ "just text"; "=value" ]
;;

let colon_value =
  perl
    "idiom/config/colon-value"
    {|([^:]*):\s*(.*)|}
    [ "key: value"; "a:b"; "Content-Type: text/html" ]
    [ "novalue" ]
;;

(* Quoted strings. *)

let double_quoted =
  perl "idiom/string/double-quoted" {|"([^"]*)"|} [ {|say "hi"|}; {|""|} ] [ "no quotes" ]
;;

let single_quoted =
  perl "idiom/string/single-quoted" {|'([^']*)'|} [ "'hi'"; "''" ] [ "no quotes" ]
;;

(* Units and versions. *)

let duration =
  perl
    "idiom/unit/duration"
    {|([0-9]+)(ms|s|m|h|d|w|y)|}
    [ "250ms"; "3h"; "1w" ]
    [ "250"; "ms" ]
;;

let dotted_version =
  perl
    "idiom/version/dotted"
    {|([0-9]+)\.([0-9]+)(\.([0-9]+))?|}
    [ "1.2"; "v10.20.30" ]
    [ "v1"; "abc" ]
;;

(* Terminal output and markup. *)

let ansi_escape =
  perl
    "idiom/terminal/ansi"
    {|\x1b\[[0-9;]*m|}
    [ "\x1b[31m"; "a\x1b[1;32mb" ]
    [ "[31m"; "plain" ]
;;

let html_entity =
  perl "idiom/html/entity" {|&(amp|lt|gt);|} [ "a &amp; b"; "&lt;" ] [ "a & b"; "&amp" ]
;;

let opam_template =
  perl
    "idiom/template/opam"
    {|%\{[^}]+\}%|}
    [ "%{ocaml:version}%"; "x %{var}% y" ]
    [ "plain"; "%{}%" ]
;;

let mustache_template =
  perl
    "idiom/template/mustache"
    {|\{\{[^}]+\}\}|}
    [ "{{name}}"; "x {{ a.b }} y" ]
    [ "{name}"; "{{}}" ]
;;

let cases =
  [ trim
  ; whitespace_run
  ; tab_separated
  ; separator_run
  ; crlf
  ; blank_line
  ; hash_comment
  ; integer
  ; float
  ; hex
  ; key_value
  ; colon_value
  ; double_quoted
  ; single_quoted
  ; duration
  ; dotted_version
  ; ansi_escape
  ; html_entity
  ; opam_template
  ; mustache_template
  ]
;;

let%test_unit "idiom pattern samples match with the intended polarity" =
  check_samples cases
;;
