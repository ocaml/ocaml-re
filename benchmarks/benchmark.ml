open Core.Std
open Core_bench.Std

let str_20_zeroes = String.make 20 '0'
let re_20_zeroes = Re.(compile (str str_20_zeroes))

let lots_of_a's =
  String.init 101 ~f:(function
      | 100 -> 'b'
      | _ -> 'a')

let lots_o_a's_re =
  Re.(seq [char 'a' ; opt (char 'a') ; char 'b'])
  |> Re.compile

let media_type_re =
  let re = Re_emacs.re ~case:true "[ \t]*\\([^ \t;]+\\)" in
  Re.(compile (seq ([start; re])))

(* Taken from https://github.com/rgrinberg/ocaml-uri/blob/903ef1010f9808d6f3f6d9c1fe4b4eabbd76082d/lib/uri.ml*)
let uri_reference =
  Re_posix.re "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
  |> Re.compile

let uris =
  [ "https://google.com"
  ; "http://yahoo.com/xxx/yyy?query=param&one=two"
  ; "file:/random_crap" ]

let benchmarks =
  [ "20 zeroes", re_20_zeroes, [str_20_zeroes]
  ; "lots of a's", lots_o_a's_re, [lots_of_a's]
  ; "media type match", media_type_re, [" foo/bar ; charset=UTF-8"]
  ; "uri", uri_reference, uris ]

let exec_bench exec name re cases =
  Bench.Test.create_group ~name (
    List.mapi cases ~f:(fun i case ->
        let name = sprintf "case %i" i in
        Bench.Test.create ~name (fun () -> ignore (exec re case))
      )
  )

let benchmarks =
  benchmarks
  |> List.map ~f:(fun (name, re, cases) ->
      Bench.Test.create_group ~name
        [ exec_bench Re.exec "exec" re cases
        ; exec_bench Re.execp "execp" re cases
        ; exec_bench Re.exec_opt "exec_opt" re cases ]
    )

let () = Command.run (Bench.make_command benchmarks)
