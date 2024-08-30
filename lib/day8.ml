open Utils
(*
   --- Day 8: Haunted Wasteland ---
   You're still riding a camel across Desert Island when you spot a sandstorm quickly approaching. When you turn to warn the Elf, she disappears before your eyes! To be fair, she had just finished warning you about ghosts a few minutes ago.

   One of the camel's pouches is labeled "maps" - sure enough, it's full of documents (your puzzle input) about how to navigate the desert. At least, you're pretty sure that's what they are; one of the documents contains a list of left/right instructions, and the rest of the documents seem to describe some kind of network of labeled nodes.

   It seems like you're meant to use the left/right instructions to navigate the network. Perhaps if you have the camel follow the same instructions, you can escape the haunted wasteland!

   After examining the maps for a bit, two nodes stick out: AAA and ZZZ. You feel like AAA is where you are now, and you have to follow the left/right instructions until you reach ZZZ.

   This format defines each node of the network individually. For example:

   RL

   AAA = (BBB, CCC)
   BBB = (DDD, EEE)
   CCC = (ZZZ, GGG)
   DDD = (DDD, DDD)
   EEE = (EEE, EEE)
   GGG = (GGG, GGG)
   ZZZ = (ZZZ, ZZZ)
   Starting with AAA, you need to look up the next element based on the next left/right instruction in your input. In this example, start with AAA and go right (R) by choosing the right element of AAA, CCC. Then, L means to choose the left element of CCC, ZZZ. By following the left/right instructions, you reach ZZZ in 2 steps.

   Of course, you might not find ZZZ right away. If you run out of left/right instructions, repeat the whole sequence of instructions as necessary: RL really means RLRLRLRLRLRLRLRL... and so on. For example, here is a situation that takes 6 steps to reach ZZZ:

   LLR

   AAA = (BBB, BBB)
   BBB = (AAA, ZZZ)
   ZZZ = (ZZZ, ZZZ)
   Starting at AAA, follow the left/right instructions. How many steps are required to reach ZZZ?
*)

type dir = L | R

module SMap = Map.Make (String)

let inf_moves l = List.to_seq l |> Seq.cycle
let uncons_inf seq = Seq.uncons seq |> Option.get

let mapping lines =
  let mapping_list =
    List.filter_map
      (fun l ->
        match String.split_on_char '=' l with
        | [ k; p ] -> (
            let key = String.trim k in
            match String.trim p |> String.split_on_char ',' with
            | [ first; second ] ->
                let first, second = (String.trim first, String.trim second) in
                Some
                  ( key,
                    ( String.sub first 1 (String.length first - 1),
                      String.sub second 0 (String.length second - 1) ) )
            | _ -> None)
        | _ -> None)
      lines
  in
  SMap.of_list mapping_list

(*
   Original Solution (pre-refactor):

   let count_steps src dest map =
    let rec count curr steps seq =
      if curr = dest then steps
      else
        let f, seq' = uncons_inf seq in
        count (SMap.find curr map |> f) (steps + 1) seq'
    in
    count src 0

   let part_one file =
     let lines = file_lines file in
     match lines with
     | h :: t ->
         let moves_seq =
           char_list_of_string h
           |> List.map (fun c ->
                 match c with
                 | 'L' -> fst
                 | 'R' -> snd
                 | _ ->
                     Invalid_argument "Unexpected char in directions string"
                     |> raise)
           |> inf_moves
         in
         count_steps "AAA" "ZZZ" (mapping t) moves_seq
     | _ -> Invalid_argument "Unexpected file format" |> raise
*)

(*
   --- Part Two ---
   The sandstorm is upon you and you aren't any closer to escaping the wasteland. You had the camel follow the instructions, but you've barely left your starting position. It's going to take significantly more steps to escape!

   What if the map isn't for people - what if the map is for ghosts? Are ghosts even bound by the laws of spacetime? Only one way to find out.

   After examining the maps a bit longer, your attention is drawn to a curious fact: the number of nodes with names ending in A is equal to the number ending in Z! If you were a ghost, you'd probably just start at every node that ends with A and follow all of the paths at the same time until they all simultaneously end up at nodes that end with Z.

   For example:

   LR

   11A = (11B, XXX)
   11B = (XXX, 11Z)
   11Z = (11B, XXX)
   22A = (22B, XXX)
   22B = (22C, 22C)
   22C = (22Z, 22Z)
   22Z = (22B, 22B)
   XXX = (XXX, XXX)
   Here, there are two starting nodes, 11A and 22A (because they both end with A). As you follow each left/right instruction, use that instruction to simultaneously navigate away from both nodes you're currently on. Repeat this process until all of the nodes you're currently on end with Z. (If only some of the nodes you're on end with Z, they act like any other node and you continue as normal.) In this example, you would proceed as follows:

   Step 0: You are at 11A and 22A.
   Step 1: You choose all of the left paths, leading you to 11B and 22B.
   Step 2: You choose all of the right paths, leading you to 11Z and 22C.
   Step 3: You choose all of the left paths, leading you to 11B and 22Z.
   Step 4: You choose all of the right paths, leading you to 11Z and 22B.
   Step 5: You choose all of the left paths, leading you to 11B and 22C.
   Step 6: You choose all of the right paths, leading you to 11Z and 22Z.
   So, in this example, you end up entirely on nodes that end in Z after 6 steps.

   Simultaneously start on every node that ends with A. How many steps does it take before you're only on nodes that end with Z?
*)

module SSet = Set.Make (String)

let sources map =
  SMap.bindings map
  |> List.filter_map (fun (x, _) ->
         if String.ends_with ~suffix:"A" x then Some x else None)

let is_Z = String.ends_with ~suffix:"Z"
let is_finished = List.for_all is_Z
let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b
let lcm_multiple completed = SMap.fold (fun _ v acc -> lcm v acc) completed 1

let count_steps_multiple srcs map =
  let pairs = List.map (fun s -> (s, s)) srcs in
  let rec run_round currs completed steps seq =
    if SMap.cardinal completed = List.length srcs then lcm_multiple completed
    else
      let f, seq' = uncons_inf seq in
      let now_completed, moving_on =
        List.partition (fun (_, s) -> is_Z s) currs
      in
      let next =
        List.map (fun (src, s) -> (src, SMap.find s map |> f)) moving_on
      in
      let completed' =
        List.fold_left
          (fun acc (src, _) -> SMap.add src steps acc)
          completed now_completed
      in
      run_round next completed' (steps + 1) seq'
  in
  run_round pairs SMap.empty 0

(* Requires: no overlapping cycles. *)
let solution file =
  let lines = file_lines file in
  match lines with
  | h :: t ->
      let moves_seq =
        char_list_of_string h
        |> List.map (fun c ->
               match c with
               | 'L' -> fst
               | 'R' -> snd
               | _ ->
                   Invalid_argument "Unexpected char in directions string"
                   |> raise)
        |> inf_moves
      in
      (mapping t, moves_seq)
  | _ -> Invalid_argument "Unexpected file format" |> raise

let part_one file =
  let map, seq = solution file in
  count_steps_multiple [ "AAA" ] map seq

let part_two file =
  let map, seq = solution file in
  count_steps_multiple (sources map) map seq
