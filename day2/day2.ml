module T = Domainslib.Task

let is_invalid_allsame str =
  let first = str.[0] in
  String.for_all (fun c -> c = first) str

(** Checks whether a number is invalid by being divided into [nb] chunks. *)
let is_invalid_by nb id_str =
  let len = String.length id_str in
  if len mod nb <> 0 then false
  else
    let chunk_size = len / nb in
    let first = String.sub id_str 0 chunk_size in
    let rest = String.sub id_str chunk_size (len - chunk_size) in
    let rec aux str =
      let len_str = String.length str in
      if len_str = 0 then true
      else
        let nth_chunk = String.sub str 0 chunk_size in
        if nth_chunk = first then
          aux (String.sub str chunk_size (len_str - chunk_size))
        else false
    in
    aux rest

let is_valid id =
  let str = string_of_int id in
  let len = String.length str in
  let rec aux i =
    if i > len then true else if is_invalid_by i str then false else aux (i + 1)
  in
  match len with
  | 0 | 1 -> true
  | 5 | 7 -> not (is_invalid_allsame str)
  | 2 -> str.[0] <> str.[1]
  | 3 ->
      let first = str.[0] in
      first <> str.[1] || first <> str.[2]
  | 4 -> str.[0] <> str.[2] || str.[1] <> str.[3]
  | 6 -> not (is_invalid_by 2 str || is_invalid_by 3 str)
  | 8 -> not (is_invalid_by 2 str)
  | 9 -> not (is_invalid_by 3 str)
  | 10 -> not (is_invalid_by 2 str || is_invalid_by 5 str)
  (* In practice there are no numbers above length 10. *)
  | _ -> aux 2

(** Sums all invalid IDs in [bot-top].
    Iterates over every number. *)
let invalids_in_range (bot, top) =
  let bot', top' = (int_of_string bot, int_of_string top) in
  let rec aux i invalids =
    if i > top' then invalids
    else if is_valid i then aux (i + 1) invalids
    else aux (i + 1) (i + invalids)
  in
  aux bot' 0

(** Sums the invalid IDs in [bot-top] of the form 111 or 2222.
    Loops at most 10 times. *)
let invalids_allsame (bot, top) =
  let bot', top' = (int_of_string bot, int_of_string top) in
  let size = String.length bot in
  let bot_first = Char.code bot.[0] - 48 in
  let top_first = Char.code top.[0] - 48 in
  let rec loop i acc =
    if i > top_first then acc
    else
      let n = int_of_string (String.init size (fun _ -> Char.chr (i + 48))) in
      if n >= bot' && n <= top' then loop (i + 1) (acc + n)
      else loop (i + 1) acc
  in
  loop bot_first 0

(** Sums the invalid IDs in [bot-top] of the form halfhalf.
    For IDs of size n, loops at most 10^(n/2) times. *)
let invalids_halves (bot, top) =
  let bot', top' = (int_of_string bot, int_of_string top) in
  let size = String.length bot in
  let power = int_of_float @@ (10. ** float (size / 2)) in
  let bot_half = bot' / power in
  let top_half = top' / power in
  let rec loop i acc =
    if i > top_half then acc
    else
      let n = (power + 1) * i in
      if n >= bot' && n <= top' then loop (i + 1) (acc + n)
      else loop (i + 1) acc
  in
  loop bot_half 0

(** Sums the invalid IDs in [bot-top] of the form thirdthirdthird.
    For IDs of size n, loops at most 10^(n/3) times. *)
let invalids_thirds (bot, top) =
  let bot', top' = (int_of_string bot, int_of_string top) in
  let size = String.length bot in
  let power = int_of_float @@ (10. ** float (2 * size / 3)) in
  let half_power = int_of_float @@ (10. ** float (size / 3)) in
  let bot_third = bot' / power in
  let top_third = top' / power in
  let rec loop i acc =
    if i > top_third then acc
    else
      let n = (power + half_power + 1) * i in
      if n >= bot' && n <= top' then loop (i + 1) (acc + n)
      else loop (i + 1) acc
  in
  loop bot_third 0

(** Sums the invalid IDs in [bot-top] of the form fifthfifthfifthfifthfifth.
    For IDs of size n, loops at most 10^(n/5) times. *)
let invalids_fifths (bot, top) =
  let bot', top' = (int_of_string bot, int_of_string top) in
  let size = String.length bot in
  let powers =
    Array.init 5 (fun i -> int_of_float @@ (10. ** float (i * size / 5)))
  in
  let coef = Array.fold_left ( + ) 0 powers in
  let bot_fifth = bot' / powers.(4) in
  let top_fifth = top' / powers.(4) in
  let rec loop i acc =
    if i > top_fifth then acc
    else
      let n = coef * i in
      if n >= bot' && n <= top' then loop (i + 1) (acc + n)
      else loop (i + 1) acc
  in
  loop bot_fifth 0

(** Sums all invalid IDs in [bot-top].
    Doesn't iterate over every number unless [bot & top] are of different sizes. *)
let invalids_in_range_smart (bot, top) =
  let size = String.length bot in
  match String.length top with
  | x when x <> size -> invalids_in_range (bot, top)
  | 0 | 1 -> 0
  | 2 | 3 | 5 | 7 -> invalids_allsame (bot, top)
  | 4 -> invalids_halves (bot, top)
  | 6 ->
      invalids_halves (bot, top)
      + invalids_thirds (bot, top)
      - invalids_allsame (bot, top)
  | 8 -> invalids_halves (bot, top)
  | 9 -> invalids_thirds (bot, top)
  | 10 ->
      invalids_halves (bot, top)
      + invalids_fifths (bot, top)
      - invalids_allsame (bot, top)
  (* In practice there are no numbers above length 10. *)
  | _ -> invalids_in_range (bot, top)

let parse line =
  String.split_on_char ',' line
  |> List.map (fun range ->
         match String.split_on_char '-' range with
         | [ left; right ] -> (left, right)
         | _ -> invalid_arg range)

let day _display pool input_buffer =
  let line = Eio.Buf_read.line input_buffer in
  let ranges = parse line in
  let result =
    T.run pool (fun () ->
        let promises =
          List.map
            (fun (bot, top) ->
              T.async pool (fun () -> invalids_in_range_smart (bot, top)))
            ranges
        in
        List.fold_left (fun sum p -> sum + T.await pool p) 0 promises)
  in
  result
