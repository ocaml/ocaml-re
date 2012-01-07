
let x = Pcre.regexp "aa?b"
let _ =
  let s = String.make (1024*1024) 'a' in
  s.[1024*1024-1] <- 'b';
  for i = 0 to 99 do
    ignore (Pcre.exec ~rex:x s)
  done
