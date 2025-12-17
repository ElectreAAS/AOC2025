open Extensions

type tile = Empty | Red | Green

let tile_to_image =
  let open Notty in
  function
  | Empty -> I.void 1 1
  | Red -> I.string (A.fg A.red) "█"
  | Green -> I.string (A.fg A.green) "X"

let matrix_to_image matrix =
  let open Notty in
  let width = Array.length matrix in
  let height = Array.length matrix.(0) in
  I.tabulate width height (fun x y -> tile_to_image matrix.(x).(y)) |> I.frame

let grid_to_image (minx, maxx, miny, maxy) tiles =
  let bigass_array =
    Array.make_matrix (maxx - minx + 1) (maxy - miny + 1) Empty
  in
  Array.iter (fun (x, y) -> bigass_array.(x - minx).(y - miny) <- Red) tiles;
  matrix_to_image bigass_array

type alignment = Vertical | Horizontal

let is_on_one_edge (x, y) ((ix, iy), (jx, jy)) =
  if (* vertical *)
     iy = jy && y = iy && x >= min ix jx && x <= max ix jx then Some Vertical
  else if (* horizontal *)
          ix = jx && x = ix && y >= min iy jy && y <= max iy jy
  then Some Horizontal
  else None

let is_on_edge (x, y) tiles =
  let size = Array.length tiles in
  let rec loop i =
    if i = size then None
    else
      let edge =
        if i = size - 1 then (tiles.(0), tiles.(size - 1))
        else (tiles.(i), tiles.(i + 1))
      in
      match is_on_one_edge (x, y) edge with
      | Some al -> Some al
      | None -> loop (i + 1)
  in
  loop 0

(** Return true iff edges actually cross (not just overlap).  *)
let are_edges_crossing ((ix, iy), (jx, jy)) ((kx, ky), (lx, ly)) =
  if ix = jx then
    (* IJ is vertical *)
    if kx = lx then (* Both vertical, no overlap *)
      false
    else (
      (* KL is horizontal *)
      assert (ky = ly);
      let smallx = min kx lx and bigx = max kx lx in
      let smally = min iy jy and bigy = max iy jy in
      smallx < ix && ix < bigx && smally < ky && ky < bigy)
  else (
    (* IJ is horizontal *)
    assert (iy = jy);
    if ky = ly then (* Both horizontal, no overlap *)
      false
    else (
      (* KL is vertical *)
      assert (kx = lx);
      let smallx = min ix jx and bigx = max ix jx in
      let smally = min ky ly and bigy = max ky ly in
      smallx < kx && kx < bigx && smally < iy && iy < bigy))

(** [ray_caster (x,y) ~minx tiles] is true iff (x,y) is inside the polygon. *)
let ray_caster (x, y) ~minx tiles =
  let size = Array.length tiles in
  let rec loop i num_crossings =
    if i = size then num_crossings mod 2 = 1
    else
      let edge =
        if i = size - 1 then (tiles.(0), tiles.(size - 1))
        else (tiles.(i), tiles.(i + 1))
      in
      if Option.is_some (is_on_one_edge (x, y) edge) then true
      else if are_edges_crossing ((x, y), (minx, y)) edge then
        loop (i + 1) (num_crossings + 1)
      else loop (i + 1) num_crossings
  in
  loop 0 0

(** Return true iff no borders are crossed.  *)
let no_border_cross (ix, iy) (jx, jy) tiles =
  let size = Array.length tiles in
  let rec loop i =
    if i = size - 2 then true
    else
      let edge =
        if i = size - 1 then (tiles.(0), tiles.(size - 1))
        else (tiles.(i), tiles.(i + 1))
      in
      if are_edges_crossing ((ix, iy), (jx, jy)) edge then false
      else loop (i + 1)
  in
  loop 0

let find_2_longest_edges tiles =
  let size = Array.length tiles in
  let distance (ix, iy) (jx, jy) =
    if ix = jx then abs (iy - jy)
    else (
      assert (iy = jy);
      abs (ix - jx))
  in
  let heap =
    let cmp (_, _, dist1) (_, _, dist2) = compare dist1 dist2 in
    BinHeap.make ~cmp size ((-1, -1), (-1, -1), -1)
  in
  let wrap = distance tiles.(0) tiles.(size - 1) in
  BinHeap.insert heap (tiles.(0), tiles.(size - 1), wrap);
  let rec loop n =
    if n = size - 2 then ()
    else
      let start = tiles.(n) and finish = tiles.(n + 1) in
      let dist = distance start finish in
      BinHeap.insert heap (start, finish, dist);
      loop (n + 1)
  in
  loop 0;
  let x, y, _ = BinHeap.extract_exn heap in
  let x', y', _ = BinHeap.extract_exn heap in
  ((x, y), (x', y'))

let do_rectangles_intersect ((small_x_1, small_y_1), (big_x_1, big_y_1))
    ((kx, ky), (lx, ly)) =
  let small_x_2, big_x_2, small_y_2, big_y_2 =
    (min kx lx, max kx lx, min ky ly, max ky ly)
  in
  let is_1_bot_of_2 () = small_y_1 > big_y_2 in
  let is_1_top_of_2 () = big_y_1 < small_y_2 in
  let is_1_left_of_2 () = big_x_1 < small_x_2 in
  let is_1_right_of_2 () = small_x_1 > big_x_2 in
  (* all false -> return true *)
  not
    (is_1_bot_of_2 () || is_1_top_of_2 () || is_1_left_of_2 ()
   || is_1_right_of_2 ())

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  (* Step 1: parse all red tiles. *)
  let tiles =
    let elapsed, array =
      Utils.with_timer (fun () ->
          Array.of_seq
            (Seq.map
               (fun line -> Scanf.sscanf line "%d,%d" (fun x y -> (x, y)))
               lines))
    in
    if display then Format.printf "Step 1 finished in %7.3fms@." elapsed;
    array
  in
  (* Step 2. find the rectangle in the middle. *)
  let ((ix, iy), (jx, jy)), ((kx, ky), (lx, ly)) =
    let elapsed, result =
      Utils.with_timer (fun () -> find_2_longest_edges tiles)
    in
    if display then Format.printf "Step 2 finished in %7.3fms@." elapsed;
    result
  in
  let rect_big_x = max ix jx in
  assert (rect_big_x = max kx lx);
  let rect_small_x = min (min ix jx) (min kx lx) in
  let rect_big_y = max iy ky in
  assert (rect_big_y = max jy ly);
  let rect_small_y = min iy ky in
  assert (rect_small_y = min jy ly);
  let size = Array.length tiles in
  (* Step 2.5: get min-maximums *)
  let minx, _miny, _maxx, _maxy =
    let elapsed, res =
      Utils.with_timer (fun () ->
          Array.fold_left
            (fun (minx, miny, maxx, maxy) (x, y) ->
              (min minx x, min miny y, max maxx x, max maxy y))
            (max_int, max_int, 0, 0) tiles)
    in
    if display then Format.printf "Step 2.5 finished in %7.3fms@." elapsed;
    res
  in
  (* Step 3: compute all possible areas, and sort them. *)
  let areas =
    let cmp (_, _, area1) (_, _, area2) = compare area1 area2 in
    BinHeap.make ~cmp (size * (size - 1) / 2) ((-1, -1), (-1, -1), -1)
  in
  let rec loop i j =
    if i = size - 1 then ()
    else if j = size then loop (i + 1) (i + 2)
    else
      let ix, iy = tiles.(i) and jx, jy = tiles.(j) in
      let area = (abs (ix - jx) + 1) * (abs (iy - jy) + 1) in
      BinHeap.insert areas ((ix, iy), (jx, jy), area);
      loop i (j + 1)
  in
  let elapsed, () = Utils.with_timer (fun () -> loop 0 1) in
  if display then Format.printf "Step 3 finished in %7.3fms@." elapsed;
  (* Step 4: for each attempted rectangle (going from biggest to smallest),
     - check if it intersects with the middle rectangle.
     - and check if the non-tile vertices are inside the polygon
     - and check if the rectangle intersects with anything
  *)
  let rec loop2 () =
    match BinHeap.extract areas with
    | None -> 0
    | Some ((ix, iy), (jx, jy), area) ->
        let kx, ky = (ix, jy) in
        let lx, ly = (jx, iy) in
        (* Format.printf "Testing: (%d,%d) & (%d,%d) (area=%d)@." ix iy jx jy area; *)
        let intersect_with_middle_hole () =
          do_rectangles_intersect
            ((rect_small_x, rect_small_y), (rect_big_x, rect_big_y))
            ((ix, iy), (jx, jy))
        in
        let opposite_angles_outside () =
          not
            (ray_caster (kx, ky) ~minx tiles && ray_caster (lx, ly) ~minx tiles)
        in
        let intersect_with_anything () =
          let b1 () = no_border_cross (ix, iy) (ix, jy) tiles in
          let b2 () = no_border_cross (jx, jy) (jx, iy) tiles in
          let b3 () = no_border_cross (ix, iy) (jx, iy) tiles in
          let b4 () = no_border_cross (jx, jy) (ix, jy) tiles in
          not (b1 () && b2 () && b3 () && b4 ())
        in
        if
          intersect_with_middle_hole ()
          || opposite_angles_outside () || intersect_with_anything ()
        then loop2 ()
        else area
    (* let condition =
       let b1 =
         let elapsed, b =
           Utils.with_timer (fun () ->
               ray_cast (x3, y3) (minx, miny, maxx, maxy) tiles)
         in
         timers.(0) <- timers.(0) +. elapsed;
         b
       in
       let b2 =
         let elapsed, b =
           Utils.with_timer (fun () ->
               ray_cast (x4, y4) (minx, miny, maxx, maxy) tiles)
         in
         timers.(1) <- timers.(1) +. elapsed;
         b
       in
       let b3 =
         let elapsed, b =
           Utils.with_timer (fun () -> border_cross (ix, iy) (ix, jy) tiles)
         in
         timers.(2) <- timers.(2) +. elapsed;
         b
       in
       let b4 =
         let elapsed, b =
           Utils.with_timer (fun () -> border_cross (jx, jy) (jx, iy) tiles)
         in
         timers.(3) <- timers.(3) +. elapsed;
         b
       in

       let b5 =
         let elapsed, b =
           Utils.with_timer (fun () -> border_cross (ix, iy) (jx, iy) tiles)
         in
         timers.(4) <- timers.(4) +. elapsed;
         b
       in
       let b6 =
         let elapsed, b =
           Utils.with_timer (fun () -> border_cross (jx, jy) (ix, jy) tiles)
         in
         timers.(5) <- timers.(5) +. elapsed;
         b
       in *)
    (* Format.printf
       "(ray %d %d -> %b) (ray %d %d -> %b) (v %b) (v %b) (h %b) (h %b)@."
       x3 y3 b1 x4 y4 b2 b3 b4 b5 b6; *)
    (* b1 && b2 && b3 && b4 && b5 && b6 *)
    (* in
       if condition then (area, n) else loop2 (n + 1) *)
  in
  let elapsed, area = Utils.with_timer (fun () -> loop2 ()) in
  if display then Format.printf "Step 4 finished in %7.3fms@." elapsed;
  (* if display then
     Format.printf
       "(b1 total %7.3fms, avg %7.3fms)\n\
        (b2 total %7.3fms, avg %7.3fms)\n\
        (b3 total %7.3fms, avg %7.3fms)\n\
        (b4 total %7.3fms, avg %7.3fms)\n\
        (b5 total %7.3fms, avg %7.3fms)\n\
        (b6 total %7.3fms, avg %7.3fms)" timers.(0)
       (timers.(0) /. float attempts)
       timers.(1)
       (timers.(1) /. float attempts)
       timers.(2)
       (timers.(2) /. float attempts)
       timers.(3)
       (timers.(3) /. float attempts)
       timers.(4)
       (timers.(4) /. float attempts)
       timers.(5)
       (timers.(5) /. float attempts); *)
  area
