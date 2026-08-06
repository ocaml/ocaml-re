module Re = Core

let of_name = function
  | "alpha" -> Some Re.alpha
  | "alnum" -> Some Re.alnum
  | "ascii" -> Some Re.ascii
  | "blank" -> Some Re.blank
  | "cntrl" -> Some Re.cntrl
  | "digit" -> Some Re.digit
  | "lower" -> Some Re.lower
  | "print" -> Some Re.print
  | "space" -> Some Re.space
  | "upper" -> Some Re.upper
  | "word" -> Some Re.wordc
  | "punct" -> Some Re.punct
  | "graph" -> Some Re.graph
  | "xdigit" -> Some Re.xdigit
  | _ -> None
;;

let parse buf =
  let accept = Parse_buffer.accept buf in
  match accept ':' with
  | false -> None
  | true ->
    let compl = accept '^' in
    (match Parse_buffer.accept_until_before buf ':' with
     | None -> raise Parse_buffer.Parse_error
     | Some cls ->
       (match of_name cls with
        | None -> raise Parse_buffer.Parse_error
        | Some posix_class ->
          if not (Parse_buffer.accept_s buf ":]") then raise Parse_buffer.Parse_error;
          Some (if compl then Re.compl [ posix_class ] else posix_class)))
;;
