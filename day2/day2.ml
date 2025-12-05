module T = Domainslib.Task

let is_invalid_by nb id_str =
  let len = String.length id_str in
  if len mod nb <> 0 then false
  else
    let chunk_size = len / nb in
    let first = String.sub id_str 0 chunk_size in
    let rest = String.sub id_str chunk_size (len - chunk_size) in
    let rec aux str =
      let len_str = String.length str in
      if len_str = 0 then true
      else
        let nth_chunk = String.sub str 0 chunk_size in
        if nth_chunk = first then
          aux (String.sub str chunk_size (len_str - chunk_size))
        else false
    in
    aux rest

let is_valid id =
  let id_str = string_of_int id in
  let len = String.length id_str in
  let rec aux i =
    if i > len then true
    else if is_invalid_by i id_str then false
    else aux (i + 1)
  in
  aux 2

let invalids_in_range (bot, top) =
  let rec aux i invalids =
    if i > top then invalids
    else if is_valid i then aux (i + 1) invalids
    else aux (i + 1) (i + invalids)
  in
  aux bot 0

let parse line =
  line |> String.split_on_char ','
  |> List.map (fun range -> Scanf.sscanf range "%d-%d" (fun x y -> (x, y)))

let day display pool input_buffer =
  let line = Eio.Buf_read.line input_buffer in
  let ranges = parse line in
  T.run pool (fun () ->
      let promises =
        List.map
          (fun (bot, top) ->
            T.async pool (fun () ->
                if display then Printf.printf "Processing %d-%d\n" bot top;
                invalids_in_range (bot, top)))
          ranges
      in
      List.fold_left (fun sum p -> sum + T.await pool p) 0 promises)
  |> string_of_int
