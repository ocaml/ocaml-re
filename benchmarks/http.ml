open Re

let space = rep blank
let crlf = str "\r\n"
let token = rep1 @@ compl [ rg '\000' '\031'; set "\127)(<>@,;:\\/[]?={}" ]
let meth = token

let version =
  let digits = rep1 digit in
  let decimal = seq [ digits; opt (seq [ char '.'; digits ]) ] in
  seq [ str "HTTP/"; decimal ]
;;

let uri = rep1 (compl [ char '\n' ])
let request_line = [ space; group meth; space; group uri; group version; space ] |> seq

let header =
  let key = group (rep1 (Re.compl [ char ':' ])) in
  let value = group (rep1 (Re.compl [ char '\n' ])) in
  seq [ space; key; space; char ':'; space; value; space; crlf ]
;;

let request' = seq [ request_line; crlf; rep header; crlf ]

module Export = struct
  let request = request'
  let request_g = request' |> no_group
  let requests = request' |> rep1
  let requests_g = request' |> no_group |> rep1
end

let requests = Stdio.In_channel.read_all "benchmarks/http-requests.txt"

let rec read_all pos re reqs =
  if pos < String.length reqs
  then (
    let g = Re.exec ~pos re reqs in
    let _, pos = Re.Group.offset g 0 in
    read_all (pos + 1) re reqs)
;;
