open Import

let exact_bits length = Re.repn (Re.set "01") length (Some length)
let anchored re = Re.seq [ Re.bos; re; Re.eos ]

let broad_then_narrow branches length () =
  (* Distinct classes all accept 'a', but cannot be factored into one shared
     prefix. After 'a', every branch has its own live continuation ID. Only
     the last branch accepts 'z'; the fixed-length tail then has few live IDs
     but must construct a different state for each position.

     A hash table retains the broad prefix's capacity and clears it at every
     tail transition. Generation stamps refill that storage only on wrap. *)
  let branch i =
    let chars = Buffer.create 17 in
    Buffer.add_char chars 'a';
    for bit = 0 to 15 do
      if i land (1 lsl bit) <> 0 then Buffer.add_char chars (Char.chr bit)
    done;
    Re.seq
      [ Re.set (Buffer.contents chars); Re.char (if i = branches - 1 then 'z' else 'b') ]
  in
  anchored (Re.seq [ Re.alt (List.init branches branch); exact_bits length ])
;;

let narrow length () =
  (* Fixed repetition expands to many distinct expression IDs, but each
     derivative has few live IDs. Stamps retain a large byte table and refill
     it every 255 clears; the hash table stays small. There are no inactive
     branches: the match traverses the entire repetition. *)
  anchored (exact_bits length)
;;

let broad_params = [ 16, 1024; 4096, 16384; 65536, 262144 ]

let broad_name branches length =
  Printf.sprintf "expression IDs/broad/%d/%d" branches length
;;

let narrow_params = [ 1024; 1000000; 8000000 ]
let narrow_name length = Printf.sprintf "expression IDs/narrow/%d" length
