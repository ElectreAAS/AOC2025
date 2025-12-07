type cell = Empty | Beam | Splitter

let parse_cell = function
  | '.' -> Empty
  | '|' -> Beam
  | '^' -> Splitter
  | c -> invalid_arg (Printf.sprintf "%c" c)

let parse_line line =
  Array.init (String.length line) (fun i -> parse_cell line.[i])

let parse_first_line line = String.index line 'S'

let step row grid =
  let _, sum =
    Array.fold_left
      (fun (i, acc) -> function
        | Empty | Splitter -> (i + 1, acc)
        | Beam -> (
            match grid.(row + 1).(i) with
            | Empty ->
                grid.(row + 1).(i) <- Beam;
                (i + 1, acc)
            | Beam -> (i + 1, acc)
            | Splitter ->
                grid.(row + 1).(i - 1) <- Beam;
                grid.(row + 1).(i + 1) <- Beam;
                (i + 1, acc + 1)))
      (0, 0) grid.(row)
  in
  sum

let day _display _pool input_buffer =
  let first_line = Eio.Buf_read.line input_buffer in
  let position = parse_first_line first_line in
  let lines = Eio.Buf_read.lines input_buffer in
  let grid = Seq.map parse_line lines |> Array.of_seq in
  let height = Array.length grid in
  grid.(0).(position) <- Beam;
  let rec loop i sum =
    if i = height - 1 then sum
    else
      let nb_splits = step i grid in
      loop (i + 1) (sum + nb_splits)
  in
  loop 0 0
