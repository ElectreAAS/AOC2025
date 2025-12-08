module T = Domainslib.Task

type box = { x : int; y : int; z : int }

let parse line = Scanf.sscanf line "%d,%d,%d" (fun x y z -> { x; y; z })
let box_to_string { x; y; z } = Format.sprintf "{%3d; %3d; %3d}" x y z
let fake_box = { x = -1; y = -1; z = -1 }

let squared_dist b1 b2 =
  let x = b1.x - b2.x in
  let y = b1.y - b2.y in
  let z = b1.z - b2.z in
  (x * x) + (y * y) + (z * z)

module Clique = Set.Make (struct
  type t = box

  let compare = compare
end)

let clique_to_string c =
  let body = Clique.fold (fun box str -> str ^ " " ^ box_to_string box) c "" in
  "#{" ^ body ^ " }"

module Graph = Set.Make (struct
  type t = Clique.t

  let compare = compare
end)

let find_only elt graph =
  let filtered = Graph.filter (fun clique -> Clique.mem elt clique) graph in
  assert (Graph.cardinal filtered = 1);
  Graph.choose filtered

let graph_to_string graph =
  let body =
    Graph.fold (fun c str -> str ^ " " ^ clique_to_string c) graph ""
  in
  "#{\n" ^ body ^ "\n}"

let connect a b graph =
  let a_clique = find_only a graph in
  if Clique.mem b a_clique then graph
  else
    let b_clique = find_only b graph in
    graph |> Graph.remove a_clique |> Graph.remove b_clique
    |> Graph.add (Clique.union a_clique b_clique)

(** Bucket sort with List.sort as inner sort.  *)
let faster_sort cmp pool max_value array =
  let size = Array.length array in
  let nb_buckets = int_of_float (sqrt (float size)) in
  let buckets = Array.make nb_buckets [] in
  let m = max_value + 1 in
  for i = 0 to size - 1 do
    let _, _, v = array.(i) in
    let index = nb_buckets * v / m in
    buckets.(index) <- array.(i) :: buckets.(index)
  done;
  T.run pool (fun () ->
      T.parallel_for ~start:0 ~finish:(nb_buckets - 1)
        ~body:(fun i -> buckets.(i) <- List.sort cmp buckets.(i))
        pool);
  let a_i = ref 0 in
  Array.iter
    (List.iter (fun x ->
         array.(!a_i) <- x;
         incr a_i))
    buckets

let day _display pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  (* Step 1: Parse all boxes. *)
  let boxes =
    let elapsed, array =
      Utils.with_timer (fun () -> Array.of_seq (Seq.map parse lines))
    in
    Format.printf "Step 1 finished in %7.3fms\n" elapsed;
    array
  in
  let nb_boxes = Array.length boxes in
  (* Step 2: Compute all distances between all boxes. *)
  let distances =
    Array.make (nb_boxes * (nb_boxes - 1) / 2) (fake_box, fake_box, -1)
  in
  let rec compute_distances counter max_dist i j =
    if i = nb_boxes - 1 then max_dist
    else if j = nb_boxes then compute_distances counter max_dist (i + 1) (i + 2)
    else
      let i_box = boxes.(i) in
      let j_box = boxes.(j) in
      let dist = squared_dist i_box j_box in
      distances.(counter) <- (i_box, j_box, dist);
      compute_distances (counter + 1) (max max_dist dist) i (j + 1)
  in
  let () =
    (* Step 2.1: Compute all distances. *)
    let elapsed, max_distance =
      Utils.with_timer (fun () -> compute_distances 0 0 0 1)
    in
    Format.printf "Step 2.1 finished in %7.3fms\n" elapsed;
    (* Step 2.2: Sort distances. *)
    let elapsed, () =
      Utils.with_timer (fun () ->
          let cmp (_, _, dist1) (_, _, dist2) = compare dist1 dist2 in
          faster_sort cmp pool max_distance distances)
    in
    Format.printf "Step 2.2 finished in %7.3fms\n" elapsed
  in
  (* Step 3: Create graph representation. *)
  let graph =
    let elapsed, graph =
      Utils.with_timer (fun () ->
          Graph.of_seq (Seq.map Clique.singleton (Array.to_seq boxes)))
    in
    Format.printf "Step 3 finished in %7.3fms\n" elapsed;
    graph
  in
  (* Step 4: Create connections. *)
  let rec connections i graph last =
    if Graph.cardinal graph = 1 then last
    else
      let a, b, _ = distances.(i) in
      connections (i + 1) (connect a b graph) (a, b)
  in
  let a, b =
    let elapsed, result =
      Utils.with_timer (fun () -> connections 0 graph (fake_box, fake_box))
    in
    Format.printf "Step 4 finished in %7.3fms\n" elapsed;
    result
  in
  (* Step 5: Multiply X of last connected boxes. *)
  a.x * b.x
