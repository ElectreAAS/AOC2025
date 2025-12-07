type cell = Empty | Beam of int | Splitter

let parse_cell = function
  | '.' -> Empty
  | '|' -> Beam 1
  | '^' -> Splitter
  | c -> invalid_arg (Printf.sprintf "%c" c)

let parse_line line =
  Array.init (String.length line) (fun i -> parse_cell line.[i])

let parse_first_line line = String.index line 'S'

let step row grid =
  Array.iteri
    (fun i -> function
      | Empty | Splitter -> ()
      | Beam n -> (
          match grid.(row + 1).(i) with
          | Empty -> grid.(row + 1).(i) <- Beam n
          | Beam m -> grid.(row + 1).(i) <- Beam (m + n)
          | Splitter -> (
              (* LEFT *)
              (match grid.(row + 1).(i - 1) with
              | Empty -> grid.(row + 1).(i - 1) <- Beam n
              | Beam m -> grid.(row + 1).(i - 1) <- Beam (m + n)
              | Splitter -> failwith "Two splitters side by side?");
              (* RIGHT *)
              match grid.(row + 1).(i + 1) with
              | Empty -> grid.(row + 1).(i + 1) <- Beam n
              | Beam m -> grid.(row + 1).(i + 1) <- Beam (m + n)
              | Splitter -> failwith "Two splitters side by side?")))
    grid.(row)

let day _display _pool input_buffer =
  let first_line = Eio.Buf_read.line input_buffer in
  let position = parse_first_line first_line in
  let lines = Eio.Buf_read.lines input_buffer in
  let grid = Seq.map parse_line lines |> Array.of_seq in
  let height = Array.length grid in
  grid.(0).(position) <- Beam 1;
  let rec loop i =
    if i < height - 1 then (
      step i grid;
      loop (i + 1))
  in
  loop 0;
  Array.fold_left
    (fun sum -> function
      | Empty -> sum
      | Beam n -> sum + n
      | Splitter -> failwith "Splitters in the last row?")
    0
    grid.(height - 1)
