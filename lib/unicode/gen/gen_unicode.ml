let pf = Stdlib.Format.fprintf

let unicode_version (t : Uucd.t) =
  List.nth (String.split_on_char ' ' t.description) 1

type rng = {
  mutable k_start : int;
  mutable k_end : int;
  mutable state : bool;
  mutable ranges : (int * int) list;
}

let mk_rng () = { k_start = 0; k_end = 0; state = false; ranges = [] }

let update_rng rng f k props =
  match (rng.state, f k props) with
  | false, false -> ()
  | true, true -> rng.k_end <- k
  | true, false ->
    rng.ranges <- (rng.k_start, rng.k_end) :: rng.ranges;
    rng.state <- false
  | false, true ->
    rng.k_start <- k;
    rng.k_end <- k;
    rng.state <- true

let ranges f (t : Uucd.t) =
  let rng = mk_rng () in
  Uucd.Cpmap.iter (fun k props -> update_rng rng f k props) t.repertoire;
  List.rev rng.ranges

let cset_punctuation t =
  let general_category = Uucd.general_category in
  let f =
   fun _ props ->
    match Uucd.find props general_category with
    | Some c -> (
      match c with
      | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po -> true
      | _ -> false)
    | _ -> false
  in
  ranges f t

let cset_printable t =
  let general_category = Uucd.general_category in
  let f =
   fun k props ->
    match Uucd.find props general_category with
    | Some c -> (
      match c with
      (* letters *)
      | `Lu | `Ll | `Lt | `Lm | `Lo
      (* marks *)
      | `Mn | `Mc | `Me
      (* numbers *)
      | `Nd | `Nl | `No
      (* punctuation *)
      | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po
      (* symbols *)
      | `Sm | `Sc | `Sk | `So ->
        true
      (* spaces *)
      | `Zs when k = 32 -> true
      | _ -> false)
    | _ -> false
  in
  ranges f t

let cset_graphic t =
  let general_category = Uucd.general_category in
  let f =
   fun _ props ->
    match Uucd.find props general_category with
    | Some c -> (
      match c with
      (* letters *)
      | `Lu | `Ll | `Lt | `Lm | `Lo
      (* marks *)
      | `Mn | `Mc | `Me
      (* numbers *)
      | `Nd | `Nl | `No
      (* punctuation *)
      | `Pc | `Pd | `Ps | `Pe | `Pi | `Pf | `Po
      (* symbols *)
      | `Sm | `Sc | `Sk | `So
      (* spaces *)
      | `Zs ->
        true
      | _ -> false)
    | _ -> false
  in
  ranges f t

let cset_control t =
  let gc = Uucd.general_category in
  let f =
   fun _ props -> match Uucd.find props gc with Some `Cc -> true | _ -> false
  in
  ranges f t

let cset_space t =
  let ws = Uucd.white_space in
  let f =
   fun _ props -> match Uucd.find props ws with Some true -> true | _ -> false
  in
  ranges f t

let cset_new_line t =
  let line_break = Uucd.line_break in
  let f =
   fun _ props ->
    match Uucd.find props line_break with
    (* see https://www.unicode.org/reports/tr14/#LB5 hard line breaks 
       and https://www.unicode.org/reports/tr14/#BK mandatory Break.*)
    | Some `CR | Some `LF | Some `NL | Some `BK -> true
    | _ -> false
  in
  ranges f t

let cset_hex_digit t =
  let hex = Uucd.hex_digit in
  let f =
   fun _ props ->
    match Uucd.find props hex with Some true -> true | _ -> false
  in
  ranges f t

let cset_wordc t =
  let alphabetic props =
    match Uucd.find props Uucd.alphabetic with Some true -> true | _ -> false
  in
  let numeric_type props =
    match Uucd.find props Uucd.numeric_type with
    | Some `None | None -> false
    | _ -> true
  in
  let is_underscore k = Int.equal k 0x005f in
  let f =
   fun k props -> alphabetic props || numeric_type props || is_underscore k
  in
  ranges f t

let cset_alnum t =
  let alphabetic props =
    match Uucd.find props Uucd.alphabetic with Some true -> true | _ -> false
  in
  let numeric_type props =
    match Uucd.find props Uucd.numeric_type with
    | Some `None | None -> false
    | _ -> true
  in
  let f = fun _ props -> alphabetic props || numeric_type props in
  ranges f t

let cset_alpha t =
  let alphabetic = Uucd.alphabetic in
  let f =
   fun _ props ->
    match Uucd.find props alphabetic with Some true -> true | _ -> false
  in
  ranges f t

let cset_upper t =
  let uppercase = Uucd.uppercase in
  let f =
   fun _ props ->
    match Uucd.find props uppercase with Some true -> true | _ -> false
  in
  ranges f t

let cset_lower t =
  let lowercase = Uucd.lowercase in
  let f =
   fun _ props ->
    match Uucd.find props lowercase with Some true -> true | _ -> false
  in
  ranges f t

let cset_calpha t =
  let gc = Uucd.general_category in
  let f =
   fun k props ->
    match (k, Uucd.find props gc) with
    | _, Some `Lu | _, Some `Ll | _, Some `Lt | _, Some `Lm | _, Some `Lo ->
      true
    | _ -> false
  in
  ranges f t

let cset_cword t =
  let alpha = Uucd.alphabetic in
  let gc = Uucd.general_category in
  let jcntl = Uucd.join_control  in
  let f =
   fun _ props ->
    match Uucd.find props alpha with
    | Some true -> true
    | _ ->
    match Uucd.find props gc with
    | Some `Nd -> true
    | _ ->
      match Uucd.find props jcntl with
      | Some true -> true
    | _ -> false
  in
  ranges f t

(* OK*)
let cset_calnum t =
  let gc = Uucd.general_category in
  let f =
   fun k props ->
    match (k, Uucd.find props gc) with
    | _, Some `Lu
    | _, Some `Ll
    | _, Some `Lt
    | _, Some `Lm
    | _, Some `Lo
    | _, Some `Nd
    | _, Some `Nl
    | _, Some `No ->
      true
    | _ -> false
  in
  ranges f t

let cset_clower t =
  let gc = Uucd.general_category in
  let f =
   fun _ props -> match Uucd.find props gc with Some `Ll -> true | _ -> false
  in
  ranges f t

let cset_cupper t =
  let gc = Uucd.general_category in
  let f =
   fun _ props -> match Uucd.find props gc with Some `Lu -> true | _ -> false
  in
  ranges f t

let cset_cdigit t =
  let nt = Uucd.numeric_type in
  let f =
   fun _ props -> match Uucd.find props nt with Some `De -> true | _ -> false
  in
  ranges f t

let sep_semicolon ppf () = pf ppf ";@ "

let pp_ml_list pp_v ppf l =
  let is_first = ref true in
  let pp_values ppf l =
    List.iter
      (fun v ->
        if !is_first then is_first := false else sep_semicolon ppf ();
        pp_v ppf v)
      l
  in
  pf ppf "[ %a ]" pp_values l

let pp_ml_array pp_v ppf l =
  let is_first = ref true in
  let pp_values pf l =
    List.iter
      (fun v ->
        if !is_first then is_first := false else sep_semicolon pf ();
        pp_v pf v)
      l
  in
  pf ppf "[| %a |]" pp_values l

let pp_hex ppf i = pf ppf "0x%04X" i
let pp_range ppf (k0, k1) = pf ppf "(%a, %a)" pp_hex k0 pp_hex k1
let pp_ranges ppf l = pf ppf "%a" (pp_ml_list pp_range) l

let pp_doc ppf doc =
  match String.equal doc "" with
  | true -> ()
  | _ ->
    let l = String.split_on_char ' ' doc in
    let pp_sep ppf () = pf ppf "@ " in
    pf ppf "@[<hov 4>(** %a *)@]"
      (Format.pp_print_list ~pp_sep Format.pp_print_string)
      l

let pp_val_with_doc_intf ppf (intf, doc) =
  pf ppf "@[<v 0>%a@,@]@[<v 0>%a@,@]" pp_doc doc intf ()

let pp_values_impl ppf l =
  List.iter (fun impl -> pf ppf "@[<v 0>@[<v 0>@[<hov 2>%a@]@,@]@,@]" impl ()) l

let pp_values_intf ppf l =
  List.iter
    (fun (intf, doc) -> pf ppf "@[<v 0>%a@,@]" pp_val_with_doc_intf (intf, doc))
    l

let pp_module_impl name ppf l =
  pf ppf "@[<v 0>@[<v 2>@[<v 0>module %s = struct@,@]@,@[<v 0>%a@]@,@]@,@]end@]"
    name pp_values_impl l

let pp_module_intf name ppf l =
  pf ppf "@[<v 0>@[<v 2>@[<v 0>module %s : sig@,@]@,@[<v 0>%a@]@,@]@,@]end@]"
    name pp_values_intf l

let pp_warning ppf () =
  pf ppf "(* WARNING do not edit. This file was automatically generated. *)"

let pp_unicode_version_impl (t : Uucd.t) ppf () =
  let version = List.nth (String.split_on_char ' ' t.description) 1 in
  pf ppf "let unicode_version = \"%s\"" version

let pp_unicode_version_intf ppf () = pf ppf "val unicode_version : string"

let pp_unicode_regexp_version_impl ppf () =
  pf ppf "let unicode_regexp_version = \"23\""

let pp_unicode_regexp_version_intf ppf () =
  pf ppf "val unicode_regexp_version : string"

let pp_prop_impl (name, l) ppf () = pf ppf "let %s = %a" name pp_ranges l
let pp_prop_val_intf name ppf () = pf ppf "val %s : (int * int) list" name

let properties t =
  [
    ( "cdigit",
      cset_cdigit t,
      "Characters that are restricted to digits which can be used in a decimal \
       radix positional numeral system and which are encoded in the standard \
       in a contiguous ascending range 0..9." );
    ("cupper", cset_cupper t, "Characters with the Lu general category.");
    ("clower", cset_clower t, "Characters with the Ll general category.");
    ( "cword",
      cset_cword t,
      "Characters that have the general_category with [< `Lu | `Ll | `Lt | `Lm \
       | `Lo | `Nd | `Nl | `No ], plus underscore." );
    ( "calpha",
      cset_calpha t,
      "Characters that have the general_category with [< `Lu | `Ll | `Lt | `Lm \
       | `Lo ]." );
    ( "calnum",
      cset_calnum t,
      "Characters that have the general_category with [< `Lu | `Ll | `Lt | `Lm \
       | `Lo | `Nd | `Nl | `No ]." );
    ( "xdigit",
      cset_hex_digit t,
      "Characters commonly used for the representation of hexadecimal numbers, \
       plus their compatibility equivalents. Property hex_digit is equal to \
       true." );
    ( "lower",
      cset_lower t,
      Printf.sprintf
        "Characters with the Lowercase property. Generated from: Ll + \
         Other_Lowercase. see {{: \
         https://www.unicode.org/versions/Unicode%s/core-spec/chapter-4/#G138691} \
         Chapter 4, Character Properties in Unicode}."
        (unicode_version t) );
    ( "upper",
      cset_upper t,
      Printf.sprintf
        "Characters with the Uppercase property. Generated from: Lu + \
         Other_Uppercase. see {{: \
         https://www.unicode.org/versions/Unicode%s/core-spec/chapter-4/#G138691} \
         Chapter 4, Character Properties in Unicode}."
        (unicode_version t) );
    ( "alpha",
      cset_alpha t,
      "Characters with the Alphabetic property. The use of the contributory \
       Other_Alphabetic property in the derivation of the Alphabetic property \
       enables the inclusion of various combining marks, such as dependent \
       vowels in many Indic scripts, which function as basic elements to spell \
       out words of those writing systems. The Alphabetic property is used in \
       tooling which assigns default primary weights for characters, for \
       generation of the DUCET table used by the Unicode Collation Algorithm \
       (UCA). For more information, see see {{: \
       https://www.unicode.org/versions/Unicode17.0.0/core-spec/chapter-4/#G138691} \
       Chapter 4, Character Properties in Unicode}." );
    ( "alnum",
      cset_alnum t,
      "Characters that have the alphabetic and numeric_type properties." );
    ( "wordc",
      cset_wordc t,
      "Characters that have the alphabetic, the numeric_type properties plus \
       underscore." );
    ( "nl",
      cset_new_line t,
      "Characters that are considered as hard line breaks and can consist of \
       BK or a Newline Function (NLF) as described in {{: \
       https://www.unicode.org/versions/latest/core-spec/chapter-5/#G10213 } \
       Section 5.8, Newline Guidelines, of Unicode}. That means characters \
       with the line_break property matching [< `BK | `CR | `LF | `NL ]." );
    ( "space",
      cset_space t,
      "Spaces, separator characters and other control characters which should \
       be treated by programming languages as \"white space\" for the purpose \
       of parsing elements (i.e. property white_space equal to true). See {{: \
       https://www.unicode.org/reports/tr44/#White_Space} White_Space}" );
    ( "cntrl",
      cset_control t,
      "Characters that have their general_category value equal to `Cc (a C0 or \
       C1 control code)." );
    ( "graph",
      cset_graphic t,
      "graph is defined as a Graphic. Such characters include letters, marks, \
       numbers, punctuation, symbols, and spaces, from categories L, M, N, P, \
       S, Zs." );
    ( "print",
      cset_printable t,
      "Printable characters. Such characters include letters, marks, numbers, \
       punctuation, symbols, and the ASCII space character." );
    ( "punct",
      cset_punctuation t,
      "Punctuation character. Such characters include punctuation from general \
       category P." );
  ]

let values t =
  [
    ( pp_unicode_version_impl t,
      pp_unicode_version_intf,
      "[unicode_version] is the Unicode version supported by the library and \
       is matching {!Uucp.unicode_version}." );
    ( pp_unicode_regexp_version_impl,
      pp_unicode_regexp_version_intf,
      "The version of the Unicode Regular Expressions to which this library \
       tries to be compliant. See {{: \
       https://www.unicode.org/reports/tr18/#C0} Unicode Regular Expressions - \
       Conformance}." );
  ]

let pp_binary_search_impl ppf () =
  pf ppf "let binary_search comp i v =@.";
  pf ppf "  let rec loop start finish =@.";
  pf ppf "    if finish < start || start > finish then false@.";
  pf ppf "    else@.";
  pf ppf "      let m = (start + finish) / 2 in@.";
  pf ppf "      match comp i v.(m) with@.";
  pf ppf "        | 0 -> true@.";
  pf ppf "        | 1 -> loop (m + 1) finish@.";
  pf ppf "        | _ -> loop start (m - 1)@.";
  pf ppf "  in@.";
  pf ppf "  loop 0 (Array.length v - 1)@."

let pp_compare_impl ppf () =
  pf ppf "let compare i (min, max) =@.";
  pf ppf "  if i >= min && i <= max then 0 else if i <= min then -1 else 1@."

let nfx_qc_prop = function
  | `NFD -> Uucd.nfd_quick_check
  | `NFC -> Uucd.nfc_quick_check
  | `NFKD -> Uucd.nfkd_quick_check
  | `NFKC -> Uucd.nfkc_quick_check

let pp_nfx ppf nfx =
  let nfx =
    match nfx with
    | `NFD -> "nfd"
    | `NFC -> "nfc"
    | `NFKD -> "nfkd"
    | `NFKC -> "nfkc"
  in
  pf ppf "%s" nfx

let pp_nfx_qc_data_impl nfx ppf t =
  let uucd_prop = nfx_qc_prop nfx in
  let f =
   fun _ props ->
    match Uucd.find props uucd_prop with
    | None -> false
    | Some `False | Some `Maybe -> false
    | Some `True -> true
  in
  let cset = ranges f t in
  pf ppf "let %a_qc_data = %a" pp_nfx nfx (pp_ml_array pp_range) cset

let pp_all_nfx_qc_data_impl ppf t =
  List.iter
    (fun nfx -> pf ppf "@[<hov 2>%a@]@." (pp_nfx_qc_data_impl nfx) t)
    [ `NFC; `NFD; `NFKC; `NFKD ]

let pp_nfx_quick_check_impl ppf t =
  pf ppf "@[%a@]@." pp_all_nfx_qc_data_impl t;
  pf ppf "@[%a@]@." pp_compare_impl ();
  pf ppf "@[%a@]@." pp_binary_search_impl ();
  pf ppf "let nfx_quick_check flag u =@.";
  pf ppf "  let cp = Uchar.to_int u in@.";
  pf ppf "  let t = match flag with@.";
  pf ppf "    | `NFD -> nfd_qc_data@.";
  pf ppf "    | `NFC -> nfc_qc_data@.";
  pf ppf "    | `NFKD -> nfkd_qc_data@.";
  pf ppf "    | `NFKC -> nfkc_qc_data@.";
  pf ppf "  in@.";
  pf ppf "  binary_search compare cp t@."

let pp_nfx_quick_check_intf ppf () =
  pf ppf "val nfx_quick_check : Uunf.form -> Uchar.t -> bool"

let simple_case_folding (t : Uucd.t) =
  let tbl = Hashtbl.create 1026 in
  let cf = Uucd.simple_case_folding in
  Uucd.Cpmap.iter
    (fun k props ->
      match Uucd.find props cf with
      | None | Some `Self -> ()
      | Some (`Cp cp) -> (
        try
          Hashtbl.find tbl cp |> fun l ->
          let l =
            let l = if List.mem cp l then l else cp :: l in
            if List.mem k l then l else k :: l
          in
          Hashtbl.replace tbl cp l
        with Not_found -> Hashtbl.add tbl cp [ k; cp ]))
    t.repertoire;
  Hashtbl.fold (fun k l acc -> (k, l) :: acc) tbl []
  |> List.sort (fun (k1, _) (k2, _) -> Int.compare k1 k2)

let pp_simple_case_fold ppf (k, l) =
  pf ppf "@[%a, %a@]@ " pp_hex k (pp_ml_list pp_hex) l

let pp_simple_case_folding_impl ppf t =
  let a = simple_case_folding t in
  pf ppf "let simple_case_folding = %a" (pp_ml_array pp_simple_case_fold) a

let pp_get_simple_case_folding_impl ppf t =
  pf ppf "@[%a@]@." pp_simple_case_folding_impl t;
  pf ppf "let get_simple_case_folding cp =@.";
  pf ppf "  try@.";
  pf ppf "    let cp =@.";
  pf ppf "      match Uucp.Case.Fold.fold (Uchar.of_int cp) with@.";
  pf ppf "      | `Self -> cp@.";
  pf ppf "      | `Uchars [ cp'] -> Uchar.to_int cp'@.";
  pf ppf "      | _ -> raise Exit@.";
  pf ppf "    in@.";
  pf ppf "    let rec loop start finish =@.";
  pf ppf "      if finish < start || start > finish then []@.";
  pf ppf "      else@.";
  pf ppf "        let m = (start + finish) / 2 in@.";
  pf ppf "        match Int.compare cp (fst simple_case_folding.(m)) with@.";
  pf ppf "        | 0 -> snd simple_case_folding.(m)@.";
  pf ppf "        | 1 -> loop (m + 1) finish@.";
  pf ppf "        | _ -> loop start (m - 1)@.";
  pf ppf "    in@.";
  pf ppf "    loop 0 (Array.length simple_case_folding - 1)@.";
  pf ppf "  with Exit -> []@."

let pp_get_simple_case_folding_intf ppf () =
  pf ppf "val get_simple_case_folding : int -> int list"

let pp_impl ppf t =
  pf ppf "@[<v 0>@[%a@]@,@[<v 0>@]@." pp_warning ();
  pf ppf "@[%a@]@." pp_values_impl
    (List.map (fun (impl, _, _) -> impl) @@ values t);
  pp_nfx_quick_check_impl ppf t;
  pp_get_simple_case_folding_impl ppf t;
  pf ppf "@[<v 0>@[<v 0>%a@]@,@]@." (pp_module_impl "Regexp")
  @@ List.map (fun (name, l, _) -> pp_prop_impl (name, l))
  @@ properties t

let pp_intf ppf t =
  pf ppf "@[<v 0>@[<v 0>@[%a@]@,@]@,@]" pp_warning ();
  pf ppf "@[%a@]@." pp_values_intf
    (List.map (fun (_, intf, doc) -> (intf, doc)) @@ values t);
  pf ppf "@[%a@]@." pp_nfx_quick_check_intf ();
  pf ppf "@[%a@]@." pp_get_simple_case_folding_intf ();
  pf ppf "@[%a@]@." (pp_module_intf "Regexp")
  @@ List.map (fun (name, _, doc) -> (pp_prop_val_intf name, doc))
  @@ properties t

let usage_msg = "gen_unicode -i <input>"
let input_file = ref ""
let output = "unicode"
let speclist = [ ("-i", Arg.Set_string input_file, "Set input file name") ]
let anon_fun = fun _ -> ()

let ucd_or_die () =
  try
    let file_in = !input_file in
    let ic = Stdlib.open_in file_in in
    let d = Uucd.decoder (`Channel ic) in
    match Uucd.decode d with
    | `Ok db -> db
    | `Error e ->
      let (l0, c0), (l1, c1) = Uucd.decoded_range d in
      Stdlib.Printf.eprintf "%s:%d.%d-%d.%d: %s\n%!" file_in l0 c0 l1 c1 e;
      Stdlib.exit 1
  with Sys_error e ->
    Stdlib.Printf.eprintf "%s\n%!" e;
    Stdlib.exit 1

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let ml = output ^ ".ml" in
  let mli = output ^ ".mli" in
  let ucd = ucd_or_die () in
  let oc_impl = Stdlib.open_out ml in
  let oc_intf = Stdlib.open_out mli in
  let ppf_impl = Stdlib.Format.formatter_of_out_channel oc_impl in
  let ppf_intf = Stdlib.Format.formatter_of_out_channel oc_intf in
  pp_impl ppf_impl ucd;
  pp_intf ppf_intf ucd;
  Stdlib.close_out oc_intf;
  Stdlib.close_out oc_impl
