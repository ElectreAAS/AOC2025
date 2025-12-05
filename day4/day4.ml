type cell = Empty | Normal_roll | Marked_for_deletion

let cell_to_string = function
  | Empty -> " "
  | Normal_roll -> "█"
  | Marked_for_deletion -> "░"

let parse_line line =
  Array.init (String.length line) (fun i ->
      match line.[i] with
      | '@' -> Normal_roll
      | '.' -> Empty
      | c -> invalid_arg (Printf.sprintf "%c" c))

let pp_line line =
  Array.iter (fun cell -> print_string (cell_to_string cell)) line;
  print_newline ()

let pp_grid grid =
  Array.iter pp_line grid;
  print_endline
    "----------------------------------------------------------------------"

let prod grid y x =
  match grid.(y).(x) with Empty -> 0 | Normal_roll | Marked_for_deletion -> 1

let is_accessible grid y x =
  let size = Array.length grid in
  assert (Array.length grid.(0) = size);
  assert (y >= 0 && y < size);
  assert (x >= 0 && x < size);
  match grid.(y).(x) with
  | Empty -> false
  | Marked_for_deletion ->
      invalid_arg (Printf.sprintf "is_accessible of marked at %d %d" y x)
  | Normal_roll ->
      let above =
        if y = 0 then 0
        else
          let top_left = if x = 0 then 0 else prod grid (y - 1) (x - 1) in
          let top_mid = prod grid (y - 1) x in
          let top_right =
            if x = size - 1 then 0 else prod grid (y - 1) (x + 1)
          in
          top_left + top_mid + top_right
      in

      let left = if x = 0 then 0 else prod grid y (x - 1) in
      let right = if x = size - 1 then 0 else prod grid y (x + 1) in
      let below =
        if y = size - 1 then 0
        else
          let bot_left = if x = 0 then 0 else prod grid (y + 1) (x - 1) in
          let bot_mid = prod grid (y + 1) x in
          let bot_right =
            if x = size - 1 then 0 else prod grid (y + 1) (x + 1)
          in
          bot_left + bot_mid + bot_right
      in
      let sum = above + left + right + below in
      sum < 4

let clean grid =
  let size = Array.length grid in
  let rec aux y x =
    if y = size then aux 0 (x + 1)
    else if x = size then ()
    else if grid.(y).(x) = Marked_for_deletion then (
      grid.(y).(x) <- Empty;
      aux (y + 1) x)
    else aux (y + 1) x
  in
  aux 0 0

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let grid = Array.of_seq (Seq.map parse_line lines) in
  let size = Array.length grid in
  let rec aux y x sum =
    if y = size then aux 0 (x + 1) sum
    else if x = size then sum
    else if is_accessible grid y x then (
      grid.(y).(x) <- Marked_for_deletion;
      aux (y + 1) x (sum + 1))
    else aux (y + 1) x sum
  in
  let rec loop2 sum =
    let res = aux 0 0 0 in
    if display then pp_grid grid;
    if res = 0 then sum
    else (
      clean grid;
      loop2 (sum + res))
  in
  loop2 0 |> string_of_int
