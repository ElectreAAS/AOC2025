open Extensions

type cell = Empty | Full | Marked_for_deletion

let cell_to_image =
  let open Notty in
  function
  | Empty -> I.string A.empty " "
  | Full -> I.string (A.fg A.pine_green) "█"
  | Marked_for_deletion ->
      I.string (A.fg (if Random.bool () then A.gold else A.red)) "█"

let parse_line line =
  Array.init (String.length line) (fun i ->
      match line.[i] with
      | '@' -> Full
      | '.' -> Empty
      | 'x' -> Marked_for_deletion
      | c -> invalid_arg (Printf.sprintf "%c" c))

let grid_to_image grid =
  let open Notty in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  I.tabulate width height (fun x y -> cell_to_image grid.(y).(x)) |> I.frame

let prod grid y x =
  match grid.(y).(x) with Empty -> 0 | Full | Marked_for_deletion -> 1

let is_accessible grid y x =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  match grid.(y).(x) with
  | Empty -> false
  | Marked_for_deletion ->
      invalid_arg (Printf.sprintf "is_accessible of marked at %d %d" y x)
  | Full ->
      let above =
        if y = 0 then 0
        else
          let top_left = if x = 0 then 0 else prod grid (y - 1) (x - 1) in
          let top_mid = prod grid (y - 1) x in
          let top_right =
            if x = width - 1 then 0 else prod grid (y - 1) (x + 1)
          in
          top_left + top_mid + top_right
      in

      let left = if x = 0 then 0 else prod grid y (x - 1) in
      let right = if x = width - 1 then 0 else prod grid y (x + 1) in
      let below =
        if y = height - 1 then 0
        else
          let bot_left = if x = 0 then 0 else prod grid (y + 1) (x - 1) in
          let bot_mid = prod grid (y + 1) x in
          let bot_right =
            if x = width - 1 then 0 else prod grid (y + 1) (x + 1)
          in
          bot_left + bot_mid + bot_right
      in
      let sum = above + left + right + below in
      sum < 4

let clean grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let rec aux y x =
    if y = height then aux 0 (x + 1)
    else if x = width then ()
    else if grid.(y).(x) = Marked_for_deletion then (
      grid.(y).(x) <- Empty;
      aux (y + 1) x)
    else aux (y + 1) x
  in
  aux 0 0

let gen_new_grid height width =
  Array.init height @@ fun _ ->
  Array.init width @@ fun _ -> if Random.float 1.0 <= 0.3 then Empty else Full

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let grid =
    if false then gen_new_grid 100 400
    else Array.of_seq (Seq.map parse_line lines)
  in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let terminal = Notty_unix.Term.create () in
  let rec mark_loop y x sum =
    if y = height then mark_loop 0 (x + 1) sum
    else if x = width then sum
    else if is_accessible grid y x then (
      grid.(y).(x) <- Marked_for_deletion;
      mark_loop (y + 1) x (sum + 1))
    else mark_loop (y + 1) x sum
  in
  let rec sweep_loop sum =
    if display then Notty_unix.Term.image terminal (grid_to_image grid);
    let res = mark_loop 0 0 0 in
    if display then (
      let timer = float res *. 0.00025 in
      Unix.sleepf timer;
      Notty_unix.Term.image terminal (grid_to_image grid);
      Unix.sleepf timer);
    if res = 0 then sum
    else (
      clean grid;
      if display then Notty_unix.Term.image terminal (grid_to_image grid);
      sweep_loop (sum + res))
  in
  let res = sweep_loop 0 in
  if display then (
    Notty_unix.Term.image terminal (grid_to_image grid);
    Unix.sleepf 1.5);
  Notty_unix.Term.release terminal;
  res
