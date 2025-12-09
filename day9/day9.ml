let day _display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let tiles =
    Array.of_seq
      (Seq.map
         (fun line -> Scanf.sscanf line "%d,%d" (fun x y -> (x, y)))
         lines)
  in
  let size = Array.length tiles in
  let rec loop i j max_area =
    if i = size - 1 then max_area
    else if j = size then loop (i + 1) (i + 2) max_area
    else
      let ix, iy = tiles.(i) and jx, jy = tiles.(j) in
      let area = (abs (ix - jx) + 1) * (abs (iy - jy) + 1) in
      loop i (j + 1) (max max_area area)
  in

  loop 0 1 0
