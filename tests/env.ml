open Fort

let id x = x
let not_found () = raise Not_found

let str_printer s = "\"" ^ String.escaped s ^ "\""
let ofs_printer (i0,i1) = Printf.sprintf "(%d,%d)" i0 i1
let list_printer f l =
   "[" ^ (String.concat "; " (List.map f l)) ^ "]"
let arr_printer f a =
   "[|" ^ (String.concat "; " (List.map f (Array.to_list a))) ^ "|]"

let arr_str_printer = arr_printer str_printer
let arr_ofs_printer = arr_printer ofs_printer

let expect_eq_bool      = expect_equal_app ~printer:string_of_bool
let expect_eq_str       = expect_equal_app ~printer:str_printer
let expect_eq_ofs       = expect_equal_app ~printer:ofs_printer
let expect_eq_arr_str   = expect_equal_app ~printer:arr_str_printer
let expect_eq_arr_ofs   = expect_equal_app ~printer:arr_ofs_printer
