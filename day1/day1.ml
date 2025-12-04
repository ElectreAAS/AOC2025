module EBR = Eio.Buf_read
open EBR.Syntax

let init_dial = 50

type instruction = Left of int | Right of int

let pp_instr = function
  | Left n -> Printf.printf "L%d" n
  | Right n -> Printf.printf "R%d" n

let parse line =
  match line.[0] with
  | 'L' -> Scanf.sscanf line "L%d" (fun n -> Left n)
  | 'R' -> Scanf.sscanf line "R%d" (fun n -> Right n)
  | c -> invalid_arg (Char.escaped c)

let go dial = function
  | Left n ->
      let raw_result = dial - n in
      let nb_clicks =
        (raw_result / -100) + if dial <> 0 && raw_result <= 0 then 1 else 0
      in
      (((raw_result mod 100) + 100) mod 100, nb_clicks)
  | Right n ->
      let raw_result = dial + n in
      let nb_clicks = raw_result / 100 in
      (raw_result mod 100, nb_clicks)

let day display _pool =
  let+ lines = EBR.lines in
  let _final_dial, result =
    Seq.fold_left
      (fun (dial, sum) line ->
        let instr = parse line in
        let new_dial, nb_clicks = go dial instr in
        if display then (
          pp_instr instr;
          Printf.printf ": %d -> %d in %d click(s)\n" dial new_dial nb_clicks);
        (new_dial, sum + nb_clicks))
      (init_dial, 0) lines
  in
  string_of_int result
