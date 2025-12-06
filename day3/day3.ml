(** [pop_at_plus n str c] removes character in position [n] and appends [c]
   to the end of [str]. *)
let pop_at_plus n str c =
  let left = String.sub str 0 n in
  let right = String.sub str (n + 1) (11 - n) in
  Printf.sprintf "%s%s%c" left right c

let attempt prev prev_str c =
  let rec aux cur cur_str i =
    if i = 12 then (cur, cur_str)
    else
      let candidate = pop_at_plus i prev_str c in
      let cand_int = int_of_string candidate in
      if cand_int > cur then aux cand_int candidate (i + 1)
      else aux cur cur_str (i + 1)
  in
  aux prev prev_str 0

let max_jolt line =
  let init_str = String.sub line 0 12 in
  let init_int = int_of_string init_str in
  let jolt =
    String.fold_left
      (fun (prev, prev_str) c -> attempt prev prev_str c)
      (init_int, init_str)
      (String.sub line 12 (String.length line - 12))
  in
  jolt

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  Seq.fold_left
    (fun sum line ->
      let jolt, _jolt_str = max_jolt line in
      if display then Printf.printf "Jolt of line %s is %d\n" line jolt;
      sum + jolt)
    0 lines
