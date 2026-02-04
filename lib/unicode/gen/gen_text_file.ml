let usage_msg = "gen_text_file -o <output>"
let output_file = ref ""
let speclist = [("-o", Arg.Set_string output_file, "Set output file name")]
let anon_fun = fun _ -> ()

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let url = Printf.sprintf "https://www.unicode.org/Public/%s/ucdxml" Uucp.unicode_version in
  let oc = open_out !output_file in
  output_string oc url;
  close_out oc
