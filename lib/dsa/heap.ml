(* Min-heap using trees *)
module H : HeapInterface.HEAP = struct
  type 'b tree = Empty | Node of 'b tree * 'b * 'b tree

  (* Each element stores the value and the size (itself and children) *)
  type 'a heap = ('a * int) tree

  let create () = Empty
  let size = function Empty -> 0 | Node (_, (_, s), _) -> s

  let num_levels = function
    | Empty -> 0
    | Node (_, (_, s), _) ->
        1 + (float_of_int s |> Float.log2 |> floor |> int_of_float)

  let is_full t =
    let levels = num_levels t in
    let n = size t in
    (2. ** float_of_int levels) -. 1. = float_of_int n

  (* Insert with heapify *)
  let rec insert t v =
    match t with
    | Empty -> Node (Empty, (v, 1), Empty)
    | Node (lt, (v', s), rt) -> (
        if (not @@ is_full t) && is_full lt then
          match insert rt v with
          | Empty -> failwith "Impossible."
          | Node (clt, (cv, cs), crt) as child ->
              if cv < v' then Node (lt, (cv, s + 1), Node (clt, (v', cs), crt))
              else Node (lt, (v', s + 1), child)
        else
          match insert lt v with
          | Empty -> failwith "Impossible."
          | Node (clt, (cv, cs), crt) as child ->
              if cv < v' then Node (Node (clt, (v', cs), crt), (cv, s + 1), rt)
              else Node (child, (v', s + 1), rt))

  let push = insert

  let rec remove_last t =
    match t with
    | Empty -> (None, Empty)
    | Node (Empty, (v, _), Empty) -> (Some v, Empty)
    | Node (lt, (v, s), rt) ->
        if is_full t || (not @@ is_full rt) then
          let h, child = remove_last rt in
          (h, Node (lt, (v, s - 1), child))
        else
          let h, child = remove_last lt in
          (h, Node (child, (v, s - 1), rt))

  let rec heapify_root t =
    match t with
    | Empty | Node (Empty, _, Empty) -> t
    | Node (Node (llt, (lv, ls), lrt), (v, s), Empty) ->
        if v <= lv then t
        else Node (heapify_root (Node (llt, (v, ls), lrt)), (lv, s), Empty)
    | Node (Empty, (_), Node (_)) ->
      failwith "Impossible for heap to have an empty left subtree and non-empty right subtree."
    | Node
        ( (Node (llt, (lv, ls), lrt) as lt),
          (v, s),
          (Node (rlt, (rv, rs), rrt) as rt) ) ->
        if v <= lv && v <= rv then t
        else if (v > lv && v <= rv) || (v > lv && lv <= rv) then
          Node (heapify_root (Node (llt, (v, ls), lrt)), (lv, s), rt)
        else Node (lt, (rv, s), heapify_root (Node (rlt, (v, rs), rrt)))

  let peek_opt = function Empty -> None | Node (_, (v, _), _) -> Some v

  let peek t =
    match peek_opt t with
    | None -> failwith "No element to peek in heap"
    | Some v -> v

  let pop_opt t =
    let res = peek_opt t in
    let remaining =
      match remove_last t with
      | None, _ -> Empty
      | Some v', Empty -> Node (Empty, (v', 1), Empty)
      | Some v', Node (lt, (_, s), rt) -> heapify_root (Node (lt, (v', s), rt))
    in
    (res, remaining)

  let pop t =
    match pop_opt t with
    | None, _ -> failwith "No element to pop in heap"
    | Some v, t' -> (v, t')
end
