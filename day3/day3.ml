let merge c1 c2 = Printf.sprintf "%c%c" c1 c2 |> int_of_string

let max_jolt line =
  let init_l = line.[0] in
  let init_r = line.[1] in
  let _final_l, _final_r, jolt =
    String.fold_left
      (fun (l, r, merged) c ->
        let m1 = merge l c in
        let m2 = merge r c in
        if merged >= m1 then if merged >= m2 then (l, r, merged) else (r, c, m2)
        else if m1 >= m2 then (l, c, m1)
        else (r, c, m2))
      (init_l, init_r, merge init_l init_r)
      (String.sub line 2 (String.length line - 2))
  in
  jolt

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  Seq.fold_left
    (fun sum line ->
      let jolt = max_jolt line in
      if display then Printf.printf "Jolt of line %s is %d\n" line jolt;
      sum + jolt)
    0 lines
  |> string_of_int
