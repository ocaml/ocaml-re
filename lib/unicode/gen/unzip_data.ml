
let usage_msg = "unzip_data  -i <input> -o <output>"
let input_file = ref ""
let output_file = ref ""
let speclist = [("-i", Arg.Set_string input_file, "Set input file name"); ("-o", Arg.Set_string output_file, "Set output file name")]
let anon_fun = fun _ -> ()

let _ =
  Arg.parse speclist anon_fun usage_msg;
  let in_file = Zip.open_in !input_file in
  let entry_name = Filename.basename !output_file in
  let entry = Zip.find_entry in_file entry_name in
  let oc = open_out_bin !output_file in
  Zip.copy_entry_to_channel in_file entry oc;
  close_out oc;
  Zip.close_in in_file
