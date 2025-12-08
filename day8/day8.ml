type box = { x : int; y : int; z : int }

let parse line = Scanf.sscanf line "%d,%d,%d" (fun x y z -> { x; y; z })
let box_to_string { x; y; z } = Format.sprintf "{%d; %d; %d}" x y z

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

let day _display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  (* Step 1: Parse all boxes. *)
  let boxes = Array.of_seq (Seq.map parse lines) in
  let nb_boxes = Array.length boxes in
  (* Step 2: Compute all distances between all boxes. *)
  let rec loop i j distances =
    if i = nb_boxes - 1 then distances
    else if j = nb_boxes then loop (i + 1) (i + 2) distances
    else
      let i_box = boxes.(i) in
      let j_box = boxes.(j) in
      let dist = squared_dist i_box j_box in
      loop i (j + 1) ((i_box, j_box, dist) :: distances)
  in
  let sorted_distances =
    loop 0 1 []
    |> List.sort (fun (_, _, dist1) (_, _, dist2) -> compare dist1 dist2)
  in
  (* Step 3: Create graph representation. *)
  let graph = Graph.of_seq (Seq.map Clique.singleton (Array.to_seq boxes)) in
  (* Step 4: Create connections. *)
  let rec loop i distances graph last =
    if Graph.cardinal graph = 1 then last
    else
      match distances with
      | [] -> failwith "Not enough distances?"
      | (a, b, _) :: distances' ->
          loop (i + 1) distances' (connect a b graph) (a, b)
  in
  let fake_box = { x = -1; y = -1; z = -1 } in
  let a, b = loop 0 sorted_distances graph (fake_box, fake_box) in
  (* Step 5: Multiply X of last connected boxes. *)
  a.x * b.x
