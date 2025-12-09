open Extensions

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

  let compare = Stdlib.compare
end)

let clique_to_string c =
  let body = Clique.fold (fun box str -> str ^ " " ^ box_to_string box) c "" in
  "#{" ^ body ^ " }"

module Graph = Set.Make (struct
  type t = Clique.t

  let compare = Stdlib.compare
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

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  (* Step 1: Parse all boxes. *)
  let boxes =
    let elapsed, array =
      Utils.with_timer (fun () -> Array.of_seq (Seq.map parse lines))
    in
    if display then Format.printf "Step 1 finished in %7.3fms\n" elapsed;
    array
  in
  let nb_boxes = Array.length boxes in
  (* Step 2: Compute all distances between all boxes. *)
  let distances =
    let cmp (_, _, dist1) (_, _, dist2) =
      (* Reverse order is intended, we want a min-heap. *)
      compare dist2 dist1
    in
    BinHeap.make ~cmp (nb_boxes * (nb_boxes - 1) / 2) (fake_box, fake_box, -1)
  in
  let rec compute_distances i j =
    if i = nb_boxes - 1 then ()
    else if j = nb_boxes then compute_distances (i + 1) (i + 2)
    else
      let i_box = boxes.(i) in
      let j_box = boxes.(j) in
      let dist = squared_dist i_box j_box in
      BinHeap.insert distances (i_box, j_box, dist);
      compute_distances i (j + 1)
  in
  (* Step 2: Compute all distances. *)
  let elapsed, () = Utils.with_timer (fun () -> compute_distances 0 1) in
  if display then Format.printf "Step 2 finished in %7.3fms\n" elapsed;
  (* Step 3: Create graph representation. *)
  let graph =
    let elapsed, graph =
      Utils.with_timer (fun () ->
          Graph.of_seq (Seq.map Clique.singleton (Array.to_seq boxes)))
    in
    if display then Format.printf "Step 3 finished in %7.3fms\n" elapsed;
    graph
  in
  (* Step 4: Create connections. *)
  let rec connections graph last =
    if Graph.cardinal graph = 1 then last
    else
      let a, b, _ = BinHeap.extract_exn distances in
      connections (connect a b graph) (a, b)
  in
  let last_a, last_b =
    let elapsed, result =
      Utils.with_timer (fun () -> connections graph (fake_box, fake_box))
    in
    if display then Format.printf "Step 4 finished in %7.3fms\n" elapsed;
    result
  in
  (* Step 5: Multiply X of last connected boxes. *)
  last_a.x * last_b.x
