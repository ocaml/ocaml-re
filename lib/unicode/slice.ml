open Import

type t =
  { s : string
  ; pos : int
  ; len : int
  }

module L = struct
  type nonrec t = t list

  let get_substring slices ~start ~stop =
    if stop = start
    then ""
    else (
      let slices =
        let rec drop slices remains =
          if remains = 0
          then slices
          else (
            match slices with
            | [] -> assert false
            | ({ s = _; pos; len } as slice) :: xs ->
              let remains' = remains - len in
              if remains' >= 0
              then drop xs remains'
              else (
                let pos = pos + remains in
                let len = len - remains in
                { slice with pos; len } :: xs))
        in
        drop slices start
      in
      let buf = Buffer.create (stop - start) in
      let rec take slices remains =
        if remains > 0
        then (
          match slices with
          | [] -> assert false
          | { s; pos; len } :: xs ->
            let remains' = remains - len in
            if remains' > 0
            then (
              Buffer.add_substring buf s pos len;
              take xs remains')
            else Buffer.add_substring buf s pos remains)
      in
      take slices (stop - start);
      Buffer.contents buf)
  ;;

  let rec drop t remains =
    if remains = 0
    then t
    else (
      match t with
      | [] -> []
      | ({ s = _; pos; len } as slice) :: t ->
        if remains >= len
        then drop t (remains - len)
        else (
          let delta = len - remains in
          { slice with pos = pos + delta; len = len - delta } :: t))
  ;;

  let drop_rev t remains =
    (* TODO Use a proper functional queue *)
    if remains = 0 then t else List.rev (drop (List.rev t) remains)
  ;;
end
