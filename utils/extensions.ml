module Notty = struct
  include Notty

  module I = struct
    include I

    (** What does this do? who knows! *)
    let vmul i n =
      let rec aux acc n = if n < 2 then acc else aux (i <-> acc) (n - 1) in
      aux i n

    (** What does this do? who knows! *)
    let hmul i n =
      let rec aux acc n = if n < 2 then acc else aux (i <|> acc) (n - 1) in
      aux i n

    (** Enclose [image] in a nice frame.
        Optional arguments [bg & fg] are applied to the box-drawing characters.*)
    let frame image =
      let h = height image and w = width image in
      let pipe = string A.empty "│" in
      let left_wall = vmul (hpad 0 1 pipe) h in
      let right_wall = vmul (hpad 1 0 pipe) h in
      let flat = hmul (string A.empty "─") (w + 2) in
      let top =
        let top_left = string A.empty "┌" in
        let top_right = string A.empty "┐" in
        top_left <|> flat <|> top_right
      in
      let empty_line = pipe <|> hmul (string A.empty " ") (w + 2) <|> pipe in
      let bot =
        let bot_left = string A.empty "└" in
        let bot_right = string A.empty "┘" in
        bot_left <|> flat <|> bot_right
      in
      top <-> empty_line
      <-> (left_wall <|> image <|> right_wall)
      <-> empty_line <-> bot
  end

  module A = struct
    include A

    let gold = rgb_888 ~r:255 ~g:199 ~b:6
    let pine_green = rgb_888 ~r:12 ~g:46 ~b:9
  end
end

module List = struct
  include List

  (** [split_at n l] returns a pair of lists:
      - the first contains the n first elements.
      - the second contains the rest, if any. *)
  let split_at n l =
    let rec aux n l before =
      match (n, l) with
      | _, [] -> (rev before, [])
      | 0, _ -> (rev before, l)
      | n, x :: xs -> aux (n - 1) xs (x :: before)
    in
    aux n l []

  (** [split_on pred l] returns a pair of lists:
    - the first contains the elements at the start of [l], up to the first element that satisfies pred (not included).
    - the second contains the elements that follow (first satisfier included). *)
  let split_on pred l =
    let rec aux l before =
      match l with
      | [] -> (rev before, [])
      | x :: xs -> if pred x then (rev before, l) else aux xs (x :: before)
    in
    aux l []
end

module Array = struct
  include Array

  (** [findi_opt f a] returns the index of the first element of the array [a]
      that satisfies the predicate [f], or [None] if there is no such index. *)
  let findi_opt f a =
    let len = length a in
    let rec loop i =
      if i = len then None else if f a.(i) then Some i else loop (i + 1)
    in
    loop 0
end
