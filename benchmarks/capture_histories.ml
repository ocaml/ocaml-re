module Re = Re_private.Re

(* Expected captures include group zero. [None] means the whole input must fail. *)
type sample =
  { input : string
  ; expected : string option list option
  }

type case =
  { name : string
  ; pattern : Re.t
  ; samples : sample list
  }

let yes input captures = { input; expected = Some (Some input :: captures) }
let no input = { input; expected = None }
let name case = "capture histories/" ^ case.name

let run case re =
  List.iter (fun { input; _ } -> ignore (Re.exec_opt re input)) case.samples
;;

(* The overlapping sets keep three alternatives alive after 'a', carrying the
   same pending group-zero history. Identical literal prefixes would be factored
   by the compiler and would not demonstrate the same opportunity. *)
let adjacent =
  { name = "adjacent"
  ; pattern =
      Re.(
        whole_string
          (alt
             [ seq [ set "ab"; char 'x' ]
             ; seq [ set "ac"; char 'y' ]
             ; seq [ set "ad"; char 'z' ]
             ]))
  ; samples =
      List.map (fun s -> yes s []) [ "ax"; "ay"; "az"; "bx"; "cy"; "dz" ]
      @ List.map no [ ""; "a"; "aw"; "by"; "ax!" ]
  }
;;

(* The middle capture interrupts the shared history: A, B, A. A one-entry memo
   cannot reuse the second A. Branch order is intentional, not an optimization. *)
let interleaved =
  { name = "interleaved"
  ; pattern = Re.(whole_string (alt [ str "ab"; group (str "ac"); str "ad" ]))
  ; samples =
      [ yes "ab" [ None ]
      ; yes "ac" [ Some "ac" ]
      ; yes "ad" [ None ]
      ; no "a"
      ; no "ae"
      ; no "ac!"
      ]
  }
;;

(* Nested captures give A, B, C, ..., C, B, A. Keep both a small example and one
   wide enough to distinguish a tiny cache from complete per-assignment sharing. *)
let nested depth =
  let inputs = ref [] in
  let literal level =
    let input = Printf.sprintf "a%03d" (List.length !inputs) in
    inputs := (input, level) :: !inputs;
    Re.str input
  in
  let rec branch level =
    if level = depth
    then literal level
    else (
      let first = literal level in
      let inner = branch (level + 1) in
      let last = literal level in
      Re.alt [ first; Re.group inner; last ])
  in
  let pattern = Re.whole_string (branch 0) in
  { name = Printf.sprintf "nested/%d" depth
  ; pattern
  ; samples =
      List.map
        (fun (input, level) ->
           yes
             input
             (List.init depth (fun group -> if group < level then Some input else None)))
        (List.rev !inputs)
      @ [ no "a"; no "a999"; no "a000!" ]
  }
;;

(* A filename classifier: fixed names need no extra captures, while rotated
   files expose both the matching branch and its date-shaped component. *)
let log_files =
  let dated compressed =
    Re.(
      seq
        [ str "app."
        ; group (repn digit 8 (Some 8))
        ; str (if compressed then ".log.gz" else ".log")
        ])
  in
  { name = "log files"
  ; pattern =
      Re.(
        whole_string
          (alt
             [ str "app.log"
             ; group (dated false)
             ; str "app.log.gz"
             ; group (dated true)
             ; str "app.log.old"
             ]))
  ; samples =
      [ yes "app.log" [ None; None; None; None ]
      ; yes "app.20260115.log" [ Some "app.20260115.log"; Some "20260115"; None; None ]
      ; yes "app.20251231.log" [ Some "app.20251231.log"; Some "20251231"; None; None ]
      ; yes "app.log.gz" [ None; None; None; None ]
      ; yes
          "app.20260115.log.gz"
          [ None; None; Some "app.20260115.log.gz"; Some "20260115" ]
      ; yes "app.log.old" [ None; None; None; None ]
      ; no "app.2026011.log"
      ; no "app.2026oops.log"
      ; no "app.log.gz.tmp"
      ; no "other.log"
      ]
  }
;;

(* A small escape-token lexer, not a complete language lexer. Numeric branches
   capture the whole token to dispatch to octal, hex, or Unicode decoding. *)
let escape_tokens =
  { name = "escape tokens"
  ; pattern =
      Re.(
        whole_string
          (alt
             [ str "\\n"
             ; group (seq [ char '\\'; repn (rg '0' '7') 3 (Some 3) ])
             ; str "\\t"
             ; group (seq [ str "\\x"; repn xdigit 2 (Some 2) ])
             ; str "\\r"
             ; group (seq [ str "\\u"; repn xdigit 4 (Some 4) ])
             ; str "\\\\"
             ; str "\\\""
             ]))
  ; samples =
      [ yes "\\n" [ None; None; None ]
      ; yes "\\t" [ None; None; None ]
      ; yes "\\r" [ None; None; None ]
      ; yes "\\\\" [ None; None; None ]
      ; yes "\\\"" [ None; None; None ]
      ; yes "\\141" [ Some "\\141"; None; None ]
      ; yes "\\000" [ Some "\\000"; None; None ]
      ; yes "\\x41" [ None; Some "\\x41"; None ]
      ; yes "\\xff" [ None; Some "\\xff"; None ]
      ; yes "\\u00E9" [ None; None; Some "\\u00E9" ]
      ; no "\\8"
      ; no "\\xGG"
      ; no "\\x1"
      ; no "\\u123"
      ; no "\\nextra"
      ; no "plain"
      ]
  }
;;

(* Static endpoints interleaved with parameterized routes. An outer capture
   identifies the dynamic route; its inner capture supplies the parameter. *)
let routes =
  { name = "routes"
  ; pattern =
      Re.(
        whole_string
          (alt
             [ str "/health"
             ; group
                 (seq
                    [ char '/'
                    ; group (rep1 (set "abcdefghijklmnopqrstuvwxyz0123456789-"))
                    ; str "/metrics"
                    ])
             ; str "/ready"
             ; group (seq [ str "/users/"; group (rep1 digit) ])
             ; str "/metrics"
             ; group
                 (seq
                    [ str "/assets/"
                    ; group (rep1 (set "abcdefghijklmnopqrstuvwxyz0123456789.-"))
                    ])
             ; str "/robots.txt"
             ]))
  ; samples =
      [ yes "/health" [ None; None; None; None; None; None ]
      ; yes "/acme/metrics" [ Some "/acme/metrics"; Some "acme"; None; None; None; None ]
      ; yes
          "/team-42/metrics"
          [ Some "/team-42/metrics"; Some "team-42"; None; None; None; None ]
      ; yes "/ready" [ None; None; None; None; None; None ]
      ; yes "/users/123" [ None; None; Some "/users/123"; Some "123"; None; None ]
      ; yes "/users/007" [ None; None; Some "/users/007"; Some "007"; None; None ]
      ; yes "/metrics" [ None; None; None; None; None; None ]
      ; yes
          "/assets/app.css"
          [ None; None; None; None; Some "/assets/app.css"; Some "app.css" ]
      ; yes
          "/assets/logo.svg"
          [ None; None; None; None; Some "/assets/logo.svg"; Some "logo.svg" ]
      ; yes "/robots.txt" [ None; None; None; None; None; None ]
      ; no "/users/"
      ; no "/users/nope"
      ; no "/foo/metrics/trailing"
      ; no "prefix/health"
      ; no "/not-found"
      ; no ""
      ]
  }
;;

let cases =
  [ adjacent; interleaved; nested 4; nested 16; log_files; escape_tokens; routes ]
;;

let%test_unit "capture-history workloads retain expected captures" =
  List.iter
    (fun case ->
       let check re =
         List.iter
           (fun { input; expected } ->
              let actual =
                Option.map
                  (fun groups -> List.init (Re.group_count re) (Re.Group.get_opt groups))
                  (Re.exec_opt re input)
              in
              if actual <> expected
              then failwith (Printf.sprintf "%s: captures for %S" case.name input))
           case.samples
       in
       let re = Re.compile case.pattern in
       check re;
       check re;
       check (Re.copy_re re);
       let forced = Re.copy_re re in
       Re.force_states forced;
       check forced)
    cases
;;

let%test_unit "capture-history workloads retain captures in bytewise streams" =
  List.iter
    (fun case ->
       let re = Re.compile case.pattern in
       List.iter
         (fun { input; expected } ->
            let rec feed state pos =
              if pos = String.length input
              then (
                match Re.Stream.Group.finalize state "!?" ~pos:1 ~len:0 with
                | No_match -> None
                | Ok groups ->
                  Some (List.init (Re.group_count re) (Re.Stream.Group.Match.get groups)))
              else (
                match
                  Re.Stream.Group.feed
                    state
                    ("!" ^ String.make 1 input.[pos] ^ "?")
                    ~pos:1
                    ~len:1
                with
                | No_match -> None
                | Ok state -> feed state (pos + 1))
            in
            let actual = feed (Re.Stream.Group.create (Re.Stream.create re)) 0 in
            if actual <> expected
            then failwith (Printf.sprintf "%s: streamed captures for %S" case.name input))
         case.samples)
    cases
;;
