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

    (** Enclose [image] in a nice frame. *)
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

  let pp oc printer = function
    | [] -> Printf.fprintf oc "[]"
    | [ x ] -> Printf.fprintf oc "[%a]" printer x
    | l ->
        let rec loop = function
          | [] -> Printf.fprintf oc "[]"
          | [ x ] -> Printf.fprintf oc "%a]" printer x
          | x :: xs ->
              Printf.fprintf oc "%a; " printer x;
              loop xs
        in
        Printf.fprintf oc "[";
        loop l
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

type order = Lesser | Equal | Greater

let compare x y = if x < y then Lesser else if x = y then Equal else Greater

module BinHeap : sig
  type 'a t
  (** A binary heap. *)

  val make : ?cmp:('a -> 'a -> order) -> int -> 'a -> 'a t
  (** [make ~cmp size init] creates a binary heap of maximum [size].
      [init] will never be seen.
      if [cmp] is not provided, it defaults to [compare], making a maxheap.
      To make a minheap, reverse the usual order. *)

  val insert : 'a t -> 'a -> unit
  val peek : 'a t -> 'a option
  val extract : 'a t -> 'a option
  val extract_exn : 'a t -> 'a
end = struct
  type 'a t = { heap : 'a array; mutable size : int; cmp : 'a -> 'a -> order }

  let make ?(cmp = compare) size init =
    let heap = Array.make size init in
    { heap; size = 0; cmp }

  let children_of index =
    let double = index lsl 1 in
    (double + 1, double + 2)

  let parent_of index = (index - 1) / 2

  let insert t elt =
    if t.size = Array.length t.heap then
      invalid_arg "Can't insert element into full binary heap";
    t.heap.(t.size) <- elt;
    let rec loop i =
      let parent = parent_of i in
      let i_value = t.heap.(i) in
      if parent < 0 || t.cmp t.heap.(parent) i_value <> Lesser then ()
      else (
        t.heap.(i) <- t.heap.(parent);
        t.heap.(parent) <- i_value;
        loop parent)
    in
    loop t.size;
    t.size <- t.size + 1;
    ()

  let peek t = match t.size with 0 -> None | _ -> Some t.heap.(0)

  let extract t =
    match t.size with
    | 0 -> None
    | 1 ->
        let result = t.heap.(0) in
        t.size <- 0;
        Some result
    | size ->
        let result = t.heap.(0) in
        t.heap.(0) <- t.heap.(size - 1);
        let rec loop i =
          let i_value = t.heap.(i) in
          let left, right = children_of i in
          if left >= t.size then ()
          else
            let bigger_child =
              if right >= t.size || t.cmp t.heap.(left) t.heap.(right) <> Lesser
              then left
              else right
            in
            if t.cmp i_value t.heap.(bigger_child) = Lesser then (
              t.heap.(i) <- t.heap.(bigger_child);
              t.heap.(bigger_child) <- i_value;
              loop bigger_child)
        in
        loop 0;
        t.size <- t.size - 1;
        Some result

  let extract_exn t =
    match extract t with
    | Some x -> x
    | None -> invalid_arg "Can't extract element from empty binary heap"
end
