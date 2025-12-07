open Extensions

type cell = Empty | Beam of int | Splitter

let cell_to_image =
  let open Notty in
  function
  | Empty -> I.void 1 1
  | Splitter -> I.string A.empty "^"
  (* FIXME: change color depending on intensity *)
  | Beam _ -> I.string A.empty "|"

let grid_to_image source_pos grid =
  let open Notty in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let top_row = I.hpad source_pos 0 (I.string A.empty "S") in
  let table = I.tabulate width height (fun x y -> cell_to_image grid.(y).(x)) in
  I.( <-> ) top_row table |> I.frame

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

let day display _pool input_buffer =
  let first_line = Eio.Buf_read.line input_buffer in
  let position = parse_first_line first_line in
  let lines = Eio.Buf_read.lines input_buffer in
  let grid = Seq.map parse_line lines |> Array.of_seq in
  let terminal = Notty_unix.Term.create () in
  if display then Notty_unix.Term.image terminal (grid_to_image position grid);
  let height = Array.length grid in
  grid.(0).(position) <- Beam 1;
  let rec loop i =
    if i < height - 1 then (
      if display then (
        Unix.sleepf 0.05;
        Notty_unix.Term.image terminal (grid_to_image position grid));
      step i grid;
      loop (i + 1))
  in
  loop 0;
  if display then (
    Notty_unix.Term.image terminal (grid_to_image position grid);
    Unix.sleepf 1.5);
  Notty_unix.Term.release terminal;
  Array.fold_left
    (fun sum -> function
      | Empty -> sum
      | Beam n -> sum + n
      | Splitter -> failwith "Splitters in the last row?")
    0
    grid.(height - 1)
