module EBR = Eio.Buf_read

let range_to_string (bot, top) = Printf.sprintf "%d-%d" bot top

let rec ranges_to_string = function
  | [] -> "∅"
  | [ range ] -> range_to_string range
  | range :: rest ->
      Printf.sprintf "%s, %s" (range_to_string range) (ranges_to_string rest)

let merge_ranges ?(suffix = []) r1 r2 =
  let b1, t1 = r1 and b2, t2 = r2 in
  if t1 < b2 then r1 :: r2 :: suffix
  else if b1 > t2 then r2 :: r1 :: suffix
  else if b1 <= b2 && t1 >= t2 then r1 :: suffix
  else if b1 >= b2 && t1 <= t2 then r2 :: suffix
  else if b1 <= b2 && t1 <= t1 then (b1, t2) :: suffix
  else (b2, t1) :: suffix

let merge u r2 =
  let rec aux u r2 =
    match u with
    | [] -> [ r2 ]
    | [ r1 ] -> merge_ranges r1 r2
    | r1 :: rest -> (
        match aux rest r2 with
        | [] -> failwith "recursive merge resulted in empty?"
        | r2' :: rest' -> merge_ranges ~suffix:rest' r1 r2')
  in
  aux u r2

let parse_range line =
  Scanf.sscanf line "%d-%d" (fun left right -> (left, right))

let parse_ranges seq =
  Seq.take_while (( <> ) "") seq
  |> Seq.map parse_range |> Seq.fold_left merge []

let rec cardinal = function
  | [] -> 0
  | (bot, top) :: rest -> top - bot + 1 + cardinal rest

let day _display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let ranges = parse_ranges lines in
  cardinal ranges |> string_of_int
