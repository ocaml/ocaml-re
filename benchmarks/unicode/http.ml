open Re_unicode.Utf8.Re

let space = rep blank
let crlf = str "\r\n" (* ok if Utf8 *)
let token = rep1 @@ compl [ rg (char '\000') (char '\031'); set "\u{007f})(<>@,;:\\/[]?={}" ]
let meth = token

let version =
  let digits = rep1 digit in
  let decimal = seq [ digits; opt (seq [ char '.' |> letter; digits ]) ] in
  seq [ str "HTTP/"; decimal ]
;;

let uri = rep1 (compl [ char '\n' |> letter ])
let request_line = [ space; group meth; space; group uri; group version; space ] |> seq

let header =
  let key = group (rep1 (compl [ char ':' |> letter ])) in
  let value = group (rep1 (compl [ char '\n' |> letter])) in
  seq [ space; key; space; char ':' |> letter; space; value; space; crlf ]
;;

let request' = seq [ request_line; crlf; rep header; crlf ]

module Export = struct
  let request = request'
  let request_g = request' |> no_group
  let requests = request' |> rep1
  let requests_g = request' |> no_group |> rep1
end

let requests =
  Stdio.In_channel.read_all "benchmarks/unicode/http-requests.txt"

let rec read_all pos re reqs =
  if pos < String.length reqs
  then (
    let g = exec ~pos re reqs in
    let _, pos = Group.offset g 0 in
    read_all (pos + 1) re reqs)
;;
