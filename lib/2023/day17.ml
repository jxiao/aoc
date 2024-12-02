(*
   --- Day 17: Clumsy Crucible ---
   The lava starts flowing rapidly once the Lava Production Facility is operational. As you leave, the reindeer offers you a parachute, allowing you to quickly reach Gear Island.

   As you descend, your bird's-eye view of Gear Island reveals why you had trouble finding anyone on your way up: half of Gear Island is empty, but the half below you is a giant factory city!

   You land near the gradually-filling pool of lava at the base of your new lavafall. Lavaducts will eventually carry the lava throughout the city, but to make use of it immediately, Elves are loading it into large crucibles on wheels.

   The crucibles are top-heavy and pushed by hand. Unfortunately, the crucibles become very difficult to steer at high speeds, and so it can be hard to go in a straight line for very long.

   To get Desert Island the machine parts it needs as soon as possible, you'll need to find the best way to get the crucible from the lava pool to the machine parts factory. To do this, you need to minimize heat loss while choosing a route that doesn't require the crucible to go in a straight line for too long.

   Fortunately, the Elves here have a map (your puzzle input) that uses traffic patterns, ambient temperature, and hundreds of other parameters to calculate exactly how much heat loss can be expected for a crucible entering any particular city block.

   For example:

   2413432311323
   3215453535623
   3255245654254
   3446585845452
   4546657867536
   1438598798454
   4457876987766
   3637877979653
   4654967986887
   4564679986453
   1224686865563
   2546548887735
   4322674655533
   Each city block is marked by a single digit that represents the amount of heat loss if the crucible enters that block. The starting point, the lava pool, is the top-left city block; the destination, the machine parts factory, is the bottom-right city block. (Because you already start in the top-left block, you don't incur that block's heat loss unless you leave that block and then return to it.)

   Because it is difficult to keep the top-heavy crucible going in a straight line for very long, it can move at most three blocks in a single direction before it must turn 90 degrees left or right. The crucible also can't reverse direction; after entering each city block, it may only turn left, continue straight, or turn right.

   One way to minimize heat loss is this path:

   2>>34^>>>1323
   32v>>>35v5623
   32552456v>>54
   3446585845v52
   4546657867v>6
   14385987984v4
   44578769877v6
   36378779796v>
   465496798688v
   456467998645v
   12246868655<v
   25465488877v5
   43226746555v>
   This path never moves more than three consecutive blocks in the same direction and incurs a heat loss of only 102.

   Directing the crucible from the lava pool to the machine parts factory, but not moving more than three consecutive blocks in the same direction, what is the least heat loss it can incur?
*)

open Utils
open Dsa.Heap.H

type dirs = Left | Right | Up | Down

module S = struct
  type t = (int * int) * (dirs * int)

  let compare = compare
end

module T = struct
  type t = int * int

  let compare = compare
end

module SSet = Set.Make (S)
module TMap = Map.Make (T)

let next (r, c) = function
  | Left -> (r, c - 1)
  | Right -> (r, c + 1)
  | Up -> (r - 1, c)
  | Down -> (r + 1, c)

let turns = function
  | Left | Right -> [ Up; Down ]
  | Up | Down -> [ Left; Right ]

let dijkstra positions dest valid min_steps =
  let rec aux h seen =
    match pop_opt h with
    | None, _ -> failwith "No solution"
    | Some (heat, (r, c), (d, s)), h' ->
        if (r, c) = dest && s >= min_steps then heat
        else if SSet.mem ((r, c), (d, s)) seen then aux h' seen
        else
          let seen' = SSet.add ((r, c), (d, s)) seen in
          let next_heap =
            d :: turns d
            |> List.filter (fun d' -> valid d d' s)
            |> List.map (fun d' ->
                   (next (r, c) d', (d', if d' = d then s + 1 else 1)))
            |> List.filter (fun ((r', c'), _) -> TMap.mem (r', c') positions)
            |> List.filter (fun ((r', c'), (d', s')) ->
                   SSet.mem ((r', c'), (d', s')) seen |> not)
            |> List.map (fun ((r', c'), (d', s')) ->
                   (heat + TMap.find (r', c') positions, (r', c'), (d', s')))
            |> List.fold_left (fun acc v -> push acc v) h'
          in
          aux next_heap seen'
  in
  aux
    (push (push (create ()) (0, (0, 0), (Right, 0))) (0, (0, 0), (Down, 0)))
    SSet.empty

let grid_pos lines =
  List.fold_left
    (fun (r, racc) line ->
      ( r + 1,
        snd
        @@ List.fold_left
             (fun (c, lacc) v -> (c + 1, TMap.add (r, c) v lacc))
             (0, racc) line ))
    (0, TMap.empty) lines
  |> snd

let part_one file =
  let lines =
    file_lines file
    |> List.map (fun s ->
           char_list_of_string s
           |> List.map (fun c -> int_of_char c - int_of_char '0'))
  in
  let nr, nc = (List.length lines, List.hd lines |> List.length) in
  let positions = grid_pos lines in
  dijkstra positions (nr - 1, nc - 1) (fun d d' s -> s < 3 || d <> d') 1

(*
   --- Part Two ---
   The crucibles of lava simply aren't large enough to provide an adequate supply of lava to the machine parts factory. Instead, the Elves are going to upgrade to ultra crucibles.

   Ultra crucibles are even more difficult to steer than normal crucibles. Not only do they have trouble going in a straight line, but they also have trouble turning!

   Once an ultra crucible starts moving in a direction, it needs to move a minimum of four blocks in that direction before it can turn (or even before it can stop at the end). However, it will eventually start to get wobbly: an ultra crucible can move a maximum of ten consecutive blocks without turning.

   In the above example, an ultra crucible could follow this path to minimize heat loss:

   2>>>>>>>>1323
   32154535v5623
   32552456v4254
   34465858v5452
   45466578v>>>>
   143859879845v
   445787698776v
   363787797965v
   465496798688v
   456467998645v
   122468686556v
   254654888773v
   432267465553v
   In the above example, an ultra crucible would incur the minimum possible heat loss of 94.

   Here's another example:

   111111111111
   999999999991
   999999999991
   999999999991
   999999999991
   Sadly, an ultra crucible would need to take an unfortunate path like this one:

   1>>>>>>>1111
   9999999v9991
   9999999v9991
   9999999v9991
   9999999v>>>>
   This route causes the ultra crucible to incur the minimum possible heat loss of 71.

   Directing the ultra crucible from the lava pool to the machine parts factory, what is the least heat loss it can incur?
*)

let part_two file =
  let lines =
    file_lines file
    |> List.map (fun s ->
           char_list_of_string s
           |> List.map (fun c -> int_of_char c - int_of_char '0'))
  in
  let nr, nc = (List.length lines, List.hd lines |> List.length) in
  let positions = grid_pos lines in
  let min_steps, max_steps = (4, 10) in
  dijkstra positions
    (nr - 1, nc - 1)
    (fun d d' s ->
      (s >= min_steps && s < max_steps)
      || (s < min_steps && d = d')
      || (s >= max_steps && d <> d'))
    4
