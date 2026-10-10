open Import
open Workload

let exponential = `Inputs "exponential complete automaton"

let cases =
  [ inputs "automata/tiny" (fun () -> Re.char 'a', [ yes "a"; no "b"; yes "!a" ])
  ; inputs "automata/class" (fun () ->
      Re.(whole_string (group (rep1 alnum))), [ yes (repeat 8192 "a1b2") ])
  ; inputs "automata/pair" (fun () ->
      Re.Perl.re "\\A((a)(b))*\\z", [ yes (repeat 16384 "ab") ])
  ; inputs "automata/rotation" (fun () ->
      Re.(whole_string (rep (group (char 'a')))), [ yes (String.make 32768 'a') ])
  ; inputs "automata/short" (fun () ->
      Re.Perl.re "([a-z]+):([0-9]+)", [ yes "abc:123"; yes "xyz:456"; no "!" ])
  ; inputs "automata/categories" (fun () ->
      ( Re.(whole_string (rep (group (set "a!\n"))))
      , List.map yes [ "a!\na!\n"; "!!!"; "aaa" ] ))
  ; inputs "automata/boundaries" (fun () ->
      ( Re.Perl.re ~opts:[ `Multiline ] "^([a-z]+)\\b.*$"
      , [ yes "a!\nbc?"; yes "\nabc"; yes "abc\n"; no "!" ] ))
  ; inputs "automata/literal256" (fun () ->
      Re.(whole_string (str Cases.all_bytes)), [ yes Cases.all_bytes ])
  ; inputs ~force:exponential "automata/wide run" (fun () ->
      ( Re.(whole_string (seq [ group (rep any); str Cases.all_bytes ]))
      , [ yes (String.make 32768 '\200' ^ Cases.all_bytes) ] ))
  ; inputs "automata/class run" (fun () ->
      ( Re.(whole_string (group (rep1 (set "word_123 test-456/"))))
      , [ yes (repeat 4096 "word_123 test-456/") ] ))
  ; inputs "automata/search" (fun () ->
      let input = repeat 4096 "word_123 test-456/" in
      Re.Perl.re "[A-Z][a-z]+:[0-9]+", [ yes (input ^ "Error:123"); no input ])
  ; inputs "automata/alternating" (fun () ->
      Re.Perl.re "\\A([a-z]+[0-9]+)+\\z", [ yes (repeat 4096 "abcdef123456") ])
  ; inputs ~force:exponential "automata/overlapping alternatives" (fun () ->
      let pattern =
        Re.alt
          (List.init 64 (fun i ->
             Re.(group (seq [ rep any; str (string_of_int i); eos ]))))
      in
      let block = repeat 128 "word_123 test-456/" in
      pattern, [ yes (block ^ "42"); no (block ^ "absent") ])
  ; inputs ~force:exponential "automata/wide alphabet" (fun () ->
      ( Re.(
          whole_string
            (seq
               [ group (rep any)
               ; char ':'
               ; alt (List.init 256 (fun i -> str (String.make 1 (Char.chr i) ^ "!")))
               ]))
      , [ yes (Cases.all_bytes ^ ":x!") ] ))
  ; inputs "automata/token classifier" (fun () ->
      let token = "abcdefghijklmnopqrstuvwxyz_0123456789" in
      ( Re.(
          whole_string
            (seq
               [ group (rep1 (set token))
               ; char ':'
               ; alt (List.init 64 (fun i -> str ("kind" ^ string_of_int i)))
               ]))
      , [ yes (token ^ ":kind42"); no (token ^ ":absent") ] ))
  ; inputs "automata/nested captures" (fun () ->
      ( Re.(nest (group (rep (group (alt [ char 'a'; str "aa" ])))))
      , List.map yes [ ""; "a"; "aa"; "aaa"; "baaa!" ] ))
  ; inputs "automata/repeated bytes4" (fun () ->
      Re.repn (Re.str Cases.all_bytes) 4 (Some 4), [ yes (repeat 4 Cases.all_bytes) ])
  ; generated
      "expression IDs/broad/256/4096"
      ~pattern:(Id_patterns.broad_then_narrow 256 4096)
      ~samples:(fun () -> [ yes ("az" ^ String.make 4096 '0') ])
  ]
  @ List.concat_map
      (fun n ->
         List.map
           (fun mode ->
              inputs (Printf.sprintf "automata/captures/%s/%d" mode n) (fun () ->
                let atom = Re.seq (List.init n (fun _ -> Re.group (Re.char 'a'))) in
                let body, count =
                  match mode with
                  | "sequential" -> atom, n
                  | "repeated" -> Re.rep atom, 4 * n
                  | _ -> Re.rep (Re.nest atom), 4 * n
                in
                Re.whole_string body, [ yes (String.make count 'a') ]))
           [ "sequential"; "repeated"; "erase" ])
      [ 16; 128; 512 ]
  @ List.map
      (fun n ->
         inputs (Printf.sprintf "automata/overlap/%d" n) (fun () ->
           let classes =
             List.init n (fun i ->
               Re.rg (Char.chr (i mod 128)) (Char.chr (128 + (i * 31 mod 128))))
           in
           Re.whole_string (Re.seq classes), [ yes (String.make n '\128') ]))
      [ 128; 256; 512; 2048 ]
  @ List.map
      (fun n ->
         inputs
           ~force:exponential
           (Printf.sprintf "automata/pathological/%d" n)
           (fun () ->
              ( Re.(seq [ rep (set "01"); char '1'; repn (set "01") n (Some n) ])
              , [ yes ("01" ^ String.make n '1') ] )))
      [ 100; 1000 ]
  @ List.map
      (fun n ->
         inputs (Printf.sprintf "automata/occupancy/%d" n) (fun () ->
           let input i = String.make 1 (Char.chr i) ^ Printf.sprintf "%03d" i in
           let pattern =
             Re.whole_string (Re.alt (List.init 256 (fun i -> Re.str (input i))))
           in
           (* Visit colors out of order, across every bitmap byte boundary. *)
           pattern, List.init n (fun j -> yes (input (j * 197 land 255)))))
      [ 8; 32; 64; 128; 256 ]
  @ List.concat_map
      (fun groups ->
         List.map
           (fun colors ->
              inputs
                ~check:(fun re ->
                  for j = 0 to colors - 1 do
                    let g = Re.exec re (List.nth Cases.duplicate_inputs j) in
                    assert (Re.Group.nb_groups g = groups + 1);
                    for i = 0 to groups do
                      assert (Re.Group.offset g i = (0, 0))
                    done
                  done)
                (Printf.sprintf "automata/duplicate status/%d/%d" groups colors)
                (fun () ->
                   let pattern =
                     Re.alt
                       [ Re.seq (List.init groups (fun _ -> Re.group Re.epsilon))
                       ; Re.str Cases.all_bytes
                       ]
                   in
                   ( pattern
                   , List.init colors (fun i -> yes (List.nth Cases.duplicate_inputs i)) )))
           [ 1; 8; 32; 128 ])
      [ 0; 1; 4; 16; 64 ]
  @ List.concat_map
      (fun width ->
         List.map
           (fun batch ->
              inputs
                (Printf.sprintf "automata/descriptor width/%d/%d" width batch)
                (fun () ->
                   let length = 32 in
                   let hex = "0123456789abcdef" in
                   let suffix i = Printf.sprintf "x%04x" i in
                   (* Different initial classes all accept '@', keeping every
                      distinct continuation live across the fixed-length field. *)
                   let branch i =
                     let chars = Buffer.create 17 in
                     Buffer.add_char chars '@';
                     for bit = 0 to 15 do
                       if i land (1 lsl bit) <> 0
                       then Buffer.add_char chars (Char.chr bit)
                     done;
                     Re.(
                       seq
                         [ set (Buffer.contents chars)
                         ; repn (set hex) length (Some length)
                         ; str (suffix i)
                         ])
                   in
                   let samples =
                     List.init batch (fun i ->
                       yes ("@" ^ String.make length hex.[i] ^ suffix (width - 1)))
                   in
                   Re.whole_string (Re.alt (List.init width branch)), samples))
           [ 1; 8 ])
      [ 16; 128; 1024; 4096 ]
  @ List.map
      (fun depth ->
         inputs (Printf.sprintf "automata/interleaved/%d" depth) (fun () ->
           let c = Capture_histories.nested depth in
           let samples =
             List.filter_map
               (fun (s : Capture_histories.sample) ->
                  if Option.is_some s.expected then Some (yes s.input) else None)
               c.samples
           in
           c.pattern, samples))
      [ 1; 4; 8; 16; 32 ]
;;
