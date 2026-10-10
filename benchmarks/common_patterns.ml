open Base
open Import
(* Common real-world regexes with yes/no samples. None use lookaround or
   backreferences. *)

type case =
  { name : string
  ; pattern : Re.t
  ; yes : string list
  ; no : string list
  ; forceable : bool
  }

let perl ?(forceable = true) name pattern yes no =
  { name; pattern = Re.Perl.re pattern; yes; no; forceable }
;;

let bs = Re.char '\\'

(* Validation patterns; these expect the whole field. *)

let email_html5 =
  (* HTML5 input type=email. *)
  perl
    "common/validation/email-html5"
    {re|^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$|re}
    [ "user@example.com"
    ; "john.doe+tag@sub.domain.co.uk"
    ; "a_b-c%d@my-host.org"
    ; "x@y.io"
    ]
    [ "user@"
    ; "@example.com"
    ; "user@example."
    ; "user@example..com"
    ; "user@@example.com"
    ; "user name@example.com"
    ; "user@-example.com"
    ]
;;

let url =
  (* Popular practical URL pattern. *)
  perl
    "common/validation/url"
    {|^(?:https?|ftp)://[^\s/$.?#].[^\s]*$|}
    [ "http://example.com"
    ; "https://example.com/path?query=1#frag"
    ; "ftp://files.example.org/pub/file.txt"
    ; "http://localhost:8080/status"
    ]
    [ "example.com"; "http://"; "mailto:user@example.com"; "http://exa mple.com" ]
;;

let ipv4 =
  perl
    "common/validation/ipv4"
    {|^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$|}
    [ "192.168.1.1"; "255.255.255.255"; "0.0.0.0"; "10.0.0.255" ]
    [ "256.1.1.1"; "192.168.1"; "192.168.1.1.1"; "999.999.999.999"; "1.2.3.4a" ]
;;

let ipv6 =
  (* Stack Overflow's all-forms IPv6 pattern. *)
  perl
    "common/validation/ipv6"
    {|^(([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]+|::(ffff(:0{1,4})?:)?((25[0-5]|(2[0-4]|1?[0-9])?[0-9])\.){3}(25[0-5]|(2[0-4]|1?[0-9])?[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1?[0-9])?[0-9])\.){3}(25[0-5]|(2[0-4]|1?[0-9])?[0-9]))$|}
    [ "::"
    ; "::1"
    ; "2001:db8::1"
    ; "fe80::1%eth0"
    ; "::ffff:192.168.1.1"
    ; "2001:0db8:85a3:0000:0000:8a2e:0370:7334"
    ]
    [ "2001:db8::1::1"; "12345::1"; "192.168.1.1"; "2001:db8::-1"; "fe80::1%" ]
;;

let mac =
  perl
    "common/validation/mac"
    {|^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$|}
    [ "00:1B:44:11:3A:B7"; "00-1B-44-11-3A-B7" ]
    [ "00:1B:44:11:3A"; "001B.4411.3AB7"; "00:1B:44:11:3A:B7:8C"; "00:1G:44:11:3A:B7" ]
;;

let iso_date =
  perl
    "common/validation/iso-date"
    {|^\d{4}-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])$|}
    [ "2026-03-29"; "1999-12-31"; "2024-02-29" ]
    [ "2026-13-01"; "2026-00-10"; "2026-03-32"; "26-03-29"; "2026/03/29" ]
;;

let time_24h =
  perl
    "common/validation/time-24h"
    {|^(?:[01]\d|2[0-3]):[0-5]\d(?::[0-5]\d)?$|}
    [ "00:00"; "23:59:59"; "07:05"; "12:30:45" ]
    [ "24:00"; "12:60"; "1:00"; "12:30:4"; "12.30" ]
;;

let iso_timestamp =
  perl
    "common/validation/iso-timestamp"
    {|^\d{4}-\d{2}-\d{2}[Tt ]\d{2}:\d{2}:\d{2}(?:\.\d+)?(?:Z|[+-]\d{2}:?\d{2})?$|}
    [ "2026-03-29T12:30:45Z"
    ; "2026-03-29 12:30:45.123+02:00"
    ; "1999-12-31t23:59:59-0500"
    ]
    [ "2026-03-29"
    ; "2026-03-29T12:30"
    ; "2026-03-29T12:30:45+2:00"
    ; "2026-03-29X12:30:45Z"
    ]
;;

let phone_us =
  perl
    "common/validation/phone-us"
    {|^(?:\+?1[-. ]?)?(?:\(\d{3}\)|\d{3})[-. ]?\d{3}[-. ]?\d{4}$|}
    [ "(555) 123-4567"
    ; "555-123-4567"
    ; "5551234567"
    ; "+1-555-123-4567"
    ; "1.555.123.4567"
    ]
    [ "555-1234"; "(555 123-4567"; "555-123-45678"; "55-123-4567" ]
;;

let credit_card =
  perl
    "common/validation/credit-card"
    {|^(?:4\d{12}(?:\d{3})?|5[1-5]\d{14}|3[47]\d{13}|6(?:011|5\d{2})\d{12})$|}
    [ "4111111111111111"; "5500000000000004"; "340000000000009"; "6011000000000004" ]
    [ "411111111111"; "41111111111111111"; "1234567890123456"; "4111-1111-1111-1111" ]
;;

let uuid =
  perl
    "common/validation/uuid"
    {|^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$|}
    [ "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
    ; "550e8400-e29b-41d4-a716-446655440000"
    ; "6ba7b810-9dad-11d1-80b4-00c04fd430c8"
    ]
    [ "f81d4fae-7dec-11d0-a765-00a0c91e6bf"
    ; "f81d4fae7dec11d0a76500a0c91e6bf6"
    ; "f81d4fae-7dec-61d0-a765-00a0c91e6bf6"
    ; "f81d4fae-7dec-11d0-c765-00a0c91e6bf6"
    ]
;;

let semver =
  (* semver.org's official pattern. *)
  perl
    "common/validation/semver"
    {|^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$|}
    [ "1.0.0"; "0.1.2"; "1.0.0-alpha.1"; "1.2.3+build.456"; "1.0.0-beta+exp.sha.5114f85" ]
    [ "1.0"; "01.0.0"; "1.0.0-"; "1.0.0+"; "v1.0.0" ]
;;

let base64 =
  perl
    "common/validation/base64"
    {|^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$|}
    [ "TWFu"; "TWE="; "aGVsbG8="; "aGVsbG8gd29ybGQ=" ]
    [ "aGVsbG8"; "aGVsbG8=="; "aGV=bG8="; "aGVs bG8=" ]
;;

let domain =
  perl
    "common/validation/domain"
    {|^(?:[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?\.)+[A-Za-z]{2,63}$|}
    [ "example.com"; "sub.domain.co.uk"; "my-host.example.org"; "a.io" ]
    [ "example"; "-example.com"; "example-.com"; "example..com"; "example.c0m" ]
;;

(* Parsing and extraction patterns; these are searched, not anchored. *)

let apache_combined =
  perl
    "common/log/apache-combined"
    {|^(\S+) (\S+) (\S+) \[([^\]]+)\] "([^"]*)" (\d{3}) (\S+)(?: "([^"]*)" "([^"]*)")?$|}
    [ {|127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326|}
    ; {|127.0.0.1 - - [10/Oct/2000:13:55:36 -0700] "GET /index.html HTTP/1.1" 200 1043 "http://example.com/start.html" "Mozilla/5.0 (X11; Linux x86_64)"|}
    ; {|10.0.0.7 - - [29/Mar/2026:09:15:00 +0000] "POST /api/items?q=1 HTTP/2.0" 201 42|}
    ]
    [ {|127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 20 2326|}
    ; {|127.0.0.1 - frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200|}
    ; {|not a log line|}
    ]
;;

let syslog_rfc3164 =
  perl
    "common/log/syslog-rfc3164"
    {|^<\d{1,3}>[A-Z][a-z]{2} {1,2}\d{1,2} \d{2}:\d{2}:\d{2} [A-Za-z0-9.-]+ [A-Za-z0-9_-]+(?:\[\d+\])?:|}
    [ {|<34>Oct 11 22:14:15 mymachine su: 'su root' failed for lonvick on /dev/pts/8|}
    ; {|<13>Feb  5 17:32:18 host.example.com named[1234]: client 192.0.2.1 query|}
    ; {|<165>Aug 24 05:34:00 myhost app: started|}
    ]
    [ {|Oct 11 22:14:15 mymachine su: 'su root' failed|}
    ; {|<34>Oct 11 22:14:15 mymachine|}
    ; {|<34>oct 11 22:14:15 mymachine su: hello|}
    ; {|<34>Oct 11 22:14 mymachine su: hello|}
    ]
;;

let structured_line =
  (* Forcing needs 821k states / 190M words. *)
  perl
    ~forceable:false
    "common/log/structured-line"
    {|^(?<timestamp>[^\ ]+ [^\ ]+)[\ ](?<level>[DIWEF])[1234]:[\ ](?<header>(?:(?:\[[^\]]*?\]|\([^\)]*?\)):[\ ])*)(?<body>.*?)[\ ]\{(?<location>[^\}]*)\}$|}
    [ {|2015-01-01 12:34:56 I1: [main]: Starting up {init.c:42}|}
    ; {|2015-01-01 12:34:56 W2: (config): [db]: failed to connect {db.c:99}|}
    ; {|2015-01-01 12:34:56 D3: some body text {/var/log/app.log:7}|}
    ]
    [ {|starting up|}
    ; {|2015-01-01 12:34:56 I1: [main]: no location|}
    ; {|2015-01-01 I1: [main]: missing seconds {init.c:42}|}
    ]
;;

let csv_row =
  perl
    "common/csv/rfc4180-row"
    {|^(?:"(?:[^"]|"")*"|[^",\n]*)(?:,(?:"(?:[^"]|"")*"|[^",\n]*))*$|}
    [ "a,b,c"; {|"hello, world",42,"say ""hi"""|}; ","; {|""|}; "a,,b" ]
    [ {|a,"b|}; {|a,b"|}; "a,b\nc" ]
;;

let json_string =
  let escape =
    Re.seq
      [ bs
      ; Re.alt
          [ Re.set {|"\/bfnrt|}; Re.seq [ Re.char 'u'; Re.repn Re.xdigit 4 (Some 4) ] ]
      ]
  in
  let body = Re.alt [ Re.compl [ Re.char '"'; bs; Re.rg '\x00' '\x1f' ]; escape ] in
  let quoted = Re.seq [ Re.char '"'; Re.rep body; Re.char '"' ] in
  { name = "common/json/string"
  ; pattern = quoted
  ; yes =
      [ {|"hello"|}
      ; {|"line\nbreak"|}
      ; {|"quote: \" and slash: \\"|}
      ; {|"\u00e9"|}
      ; {|""|}
      ]
  ; no = [ {|"unterminated|}; {|"bad \q"|}; {|"bad \u12"|}; "plain" ]
  ; forceable = true
  }
;;

let json_number =
  perl
    "common/json/number"
    {|^-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?$|}
    [ "0"; "-0"; "42"; "-12.5"; "6.022e23"; "1E-9"; "0.5" ]
    [ "01"; "+1"; ".5"; "1."; "1e"; "-" ]
;;

let html_tag =
  perl
    "common/html/tag"
    {|</?([A-Za-z][A-Za-z0-9]*)\b[^>]*?/?>|}
    [ {|<div class="x">|}; "<br/>"; "</p>"; {|<a href="https://example.com">|} ]
    [ "<>"; "<1tag>"; "plain text"; "a < b > c" ]
;;

let html_comment =
  perl
    "common/html/comment"
    {|(?s)<!--.*?-->|}
    [ "<!-- a comment -->"; "<!---->"; "before <!--\nmulti\nline\n--> after" ]
    [ "<!-- unterminated"; "<--->"; "no comments here" ]
;;

let markdown_link =
  (* Forcing does not finish in several GiB. *)
  perl
    ~forceable:false
    "common/markdown/link"
    {|\[([^\]]*)\]\(([^)\s]+)(?:\s+"([^"]*)")?\)|}
    [ "[text](https://example.com)"
    ; {|[a](b "title")|}
    ; "see [docs](docs/api.md#top) for details"
    ]
    [ "[text]("; "[text] (url)"; "[link]" ]
;;

let c_style_comment =
  (* "Unrolling the loop" block comment. *)
  perl
    "common/comment/c-style"
    {|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/|}
    [ "/* hello */"; "/* a * b ** c */"; "/*\n multi\nline\n*/" ]
    [ "/* unterminated"; "// line comment"; "/*/" ]
;;

let code_keywords =
  perl
    "common/code/keywords"
    {|\b(?:abstract|arguments|await|boolean|break|byte|case|catch|char|class|const|continue|debugger|default|delete|do|double|else|enum|eval|export|extends|final|finally|float|for|function|goto|if|implements|import|in|instanceof|int|interface|let|long|native|new|package|private|protected|public|return|short|static|super|switch|synchronized|this|throw|transient|try|typeof|var|void|volatile|while|with|yield)\b|}
    [ "const x = function() { return 1; }"
    ; "if (a instanceof B) { while (x) { break; } }"
    ; "export default class Foo extends Bar {}"
    ]
    [ "constant = 1"; "my_return = 2"; "iffy"; "identifier" ]
;;

let lexer =
  perl
    "common/code/lexer"
    {|//[^\n]*|/\*[^*]*\*+(?:[^/*][^*]*\*+)*/|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|[A-Za-z_$][A-Za-z0-9_$]*|\d+(?:\.\d+)?(?:[eE][+-]?\d+)?|==|!=|<=|>=|&&|\|\||[-+*/%<>=!&|^~?:;,.(){}\[\]]|}
    [ "int x = 42; // done"; {|let s = 'x'; return 1.5e3;|}; "/* c */ x"; "a && b || c" ]
    [ "@"; "`"; "#" ]
;;

let number_literals =
  perl
    "common/code/number-literals"
    {|^[+-]?(?:0[xX][0-9a-fA-F]+|0[bB][01]+|0[oO][0-7]+|\d+(?:\.\d*)?(?:[eE][+-]?\d+)?|\.\d+(?:[eE][+-]?\d+)?)[uUlLfFdD]?$|}
    [ "42"; "-3.14"; "+1.0e-10"; "0xFF"; "0b1010"; "0o17"; "123L"; ".5"; "1." ]
    [ "0x"; "0b2"; "1.2.3"; "--1"; "1e" ]
;;

let roman =
  perl
    "common/number/roman"
    {|^M{0,4}(?:CM|CD|D?C{0,3})(?:XC|XL|L?X{0,3})(?:IX|IV|V?I{0,3})$|}
    [ "MMXXVI"; "IV"; "MCMXCIV"; "XLII"; "MMMCMXCIX" ]
    [ "IIII"; "VV"; "IC"; "MMMMM"; "abc" ]
;;

let money =
  perl
    "common/money/usd"
    {|^\$\d{1,3}(?:,\d{3})*(?:\.\d{2})?$|}
    [ "$5"; "$19.99"; "$1,234,567.89"; "$100.50" ]
    [ "$12345"; "5.00"; "$1,23.45"; "$1.234" ]
;;

let docker_reference =
  perl
    "common/docker/reference"
    {|^(?:[a-zA-Z0-9]+(?:[._-][a-zA-Z0-9]+)*(?::\d+)?/)*[a-z0-9]+(?:[._-][a-z0-9]+)*(?::[\w][\w.-]{0,127})?(?:@sha256:[a-f0-9]{64})?$|}
    [ "nginx"
    ; "library/nginx:1.25.3"
    ; "ghcr.io/owner/app:v1.2.3"
    ; "registry.example.com:5000/team/app@sha256:0000000000000000000000000000000000000000000000000000000000000000"
    ]
    [ "Nginx"; "nginx:"; "nginx@sha256:abc"; "-nginx"; "nginx//app" ]
;;

let aws_access_key =
  perl
    "common/secret/aws-access-key"
    {|(?:AKIA|ASIA|AROA|AIDA)[A-Z0-7]{16}|}
    [ "AKIAIOSFODNN7EXAMPLE"; "ASIAIOSFODNN7EXAMPLE"; "key = AKIAIOSFODNN7EXAMPLE" ]
    [ "AKIAIOSFODNN8EXAMPLE"; "BKIAIOSFODNN7EXAMPLE"; "AKIAIOSFODNN7EXAMPL" ]
;;

let api_key_line =
  perl
    "common/secret/api-key-line"
    {|(?i:(?:api[_-]?key|secret|token)\s*[=:]\s*["']?[A-Za-z0-9_\-./+]{16,})|}
    [ {|api_key = "AKIAIOSFODNN7EXAMPLE"|}
    ; "SECRET: abcdefghijklmnop"
    ; {|token='xoxb-123456789012-abcdef'|}
    ]
    [ {|api_key = "short"|}; {|secret = "abc"|}; "nothing to see here" ]
;;

let youtube_id =
  perl
    "common/youtube/video-id"
    {|(?:youtube\.com/(?:watch\?v=|embed/|shorts/)|youtu\.be/)([A-Za-z0-9_-]{11})|}
    [ "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
    ; "https://youtu.be/dQw4w9WgXcQ"
    ; "https://youtube.com/shorts/dQw4w9WgXcQ"
    ]
    [ "https://vimeo.com/12345"
    ; "https://youtube.com/watch?v=short"
    ; "https://youtube.com/"
    ]
;;

let us_date =
  perl
    "common/date/us-slash"
    {|\b(?:0?[1-9]|1[0-2])/(?:0?[1-9]|[12][0-9]|3[01])/(?:\d{4}|\d{2})\b|}
    [ "03/29/2026"; "3/9/26"; "due on 12/31/1999" ]
    [ "13/01/2026"; "03/32/2026"; "2026-03-29" ]
;;

let cases =
  [ email_html5
  ; url
  ; ipv4
  ; ipv6
  ; mac
  ; iso_date
  ; time_24h
  ; iso_timestamp
  ; phone_us
  ; credit_card
  ; uuid
  ; semver
  ; base64
  ; domain
  ; apache_combined
  ; syslog_rfc3164
  ; structured_line
  ; csv_row
  ; json_string
  ; json_number
  ; html_tag
  ; html_comment
  ; markdown_link
  ; c_style_comment
  ; code_keywords
  ; lexer
  ; number_literals
  ; roman
  ; money
  ; docker_reference
  ; aws_access_key
  ; api_key_line
  ; youtube_id
  ; us_date
  ]
;;

let%test_unit "common pattern samples match with the intended polarity" =
  let failures =
    List.concat_map cases ~f:(fun { name; pattern; yes; no; forceable = _ } ->
      let re = Re.compile pattern in
      let yes_failures =
        List.filter_map yes ~f:(fun input ->
          if Re.execp re input then None else Some (Printf.sprintf "%s: %S" name input))
      in
      let no_failures =
        List.filter_map no ~f:(fun input ->
          if Re.execp re input then Some (Printf.sprintf "%s: %S" name input) else None)
      in
      List.map yes_failures ~f:(fun failure -> "missing match: " ^ failure)
      @ List.map no_failures ~f:(fun failure -> "unexpected match: " ^ failure))
  in
  if not (List.is_empty failures) then failwith (String.concat ~sep:"\n" failures)
;;
