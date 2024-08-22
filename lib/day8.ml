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
