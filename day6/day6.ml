open Extensions

module Operation = struct
  type t = Plus | Mult

  let neutral = function Plus -> 0 | Mult -> 1
  let math_of_t = function Plus -> ( + ) | Mult -> ( * )
  let parse = function "*" -> Some Mult | "+" -> Some Plus | _ -> None
  let parse_line line = String.split_on_char ' ' line |> List.filter_map parse
  let to_verb = function Plus -> "Adding" | Mult -> "Multiplying"

  let apply op (super_column : int list) =
    List.fold_left (math_of_t op) (neutral op) super_column
end

let day display _pool input_buffer =
  let lines = Eio.Buf_read.lines input_buffer in
  let nums_list, ops =
    Seq.fold_left
      (fun (nums, ops) line ->
        match Operation.parse_line line with
        | [] -> (line :: nums, ops)
        | ops' ->
            assert (List.is_empty ops);
            (nums, ops'))
      ([], []) lines
  in
  let nums_raw = Array.of_list nums_list in
  let size = Array.length nums_raw in
  let first_size = String.length nums_raw.(0) in
  for i = 1 to size - 1 do
    assert (String.length nums_raw.(i) = first_size)
  done;
  let rec loop current_col previous_cols i =
    if i < 0 then current_col :: previous_cols
    else
      let column =
        Array.fold_left
          (fun acc line -> Printf.sprintf "%c%s" line.[i] acc)
          "" nums_raw
      in
      match int_of_string_opt (String.trim column) with
      | None -> loop [] (current_col :: previous_cols) (i - 1)
      | Some n -> loop (n :: current_col) previous_cols (i - 1)
  in
  let nums = loop [] [] (first_size - 1) in
  List.fold_left2
    (fun sum op super_column ->
      if display then
        Printf.printf "%s %a, current sum is %d\n%!" (Operation.to_verb op)
          (fun oc l -> List.pp oc (fun oc n -> Printf.fprintf oc "%d" n) l)
          super_column sum;
      sum + Operation.apply op super_column)
    0 ops nums
