(*
   --- Day 23: A Long Walk ---
   The Elves resume water filtering operations! Clean water starts flowing over the edge of Island Island.

   They offer to help you go over the edge of Island Island, too! Just hold on tight to one end of this impossibly long rope and they'll lower you down a safe distance from the massive waterfall you just created.

   As you finally reach Snow Island, you see that the water isn't really reaching the ground: it's being absorbed by the air itself. It looks like you'll finally have a little downtime while the moisture builds up to snow-producing levels. Snow Island is pretty scenic, even without any snow; why not take a walk?

   There's a map of nearby hiking trails (your puzzle input) that indicates paths (.), forest (#), and steep slopes (^, >, v, and <).

   For example:

   #.#####################
   #.......#########...###
   #######.#########.#.###
   ###.....#.>.>.###.#.###
   ###v#####.#v#.###.#.###
   ###.>...#.#.#.....#...#
   ###v###.#.#.#########.#
   ###...#.#.#.......#...#
   #####.#.#.#######.#.###
   #.....#.#.#.......#...#
   #.#####.#.#.#########v#
   #.#...#...#...###...>.#
   #.#.#v#######v###.###v#
   #...#.>.#...>.>.#.###.#
   #####v#.#.###v#.#.###.#
   #.....#...#...#.#.#...#
   #.#########.###.#.#.###
   #...###...#...#...#.###
   ###.###.#.###v#####v###
   #...#...#.#.>.>.#.>.###
   #.###.###.#.###.#.#v###
   #.....###...###...#...#
   #####################.#
   You're currently on the single path tile in the top row; your goal is to reach the single path tile in the bottom row. Because of all the mist from the waterfall, the slopes are probably quite icy; if you step onto a slope tile, your next step must be downhill (in the direction the arrow is pointing). To make sure you have the most scenic hike possible, never step onto the same tile twice. What is the longest hike you can take?

   In the example above, the longest hike you can take is marked with O, and your starting position is marked S:

   #S#####################
   #OOOOOOO#########...###
   #######O#########.#.###
   ###OOOOO#OOO>.###.#.###
   ###O#####O#O#.###.#.###
   ###OOOOO#O#O#.....#...#
   ###v###O#O#O#########.#
   ###...#O#O#OOOOOOO#...#
   #####.#O#O#######O#.###
   #.....#O#O#OOOOOOO#...#
   #.#####O#O#O#########v#
   #.#...#OOO#OOO###OOOOO#
   #.#.#v#######O###O###O#
   #...#.>.#...>OOO#O###O#
   #####v#.#.###v#O#O###O#
   #.....#...#...#O#O#OOO#
   #.#########.###O#O#O###
   #...###...#...#OOO#O###
   ###.###.#.###v#####O###
   #...#...#.#.>.>.#.>O###
   #.###.###.#.###.#.#O###
   #.....###...###...#OOO#
   #####################O#
   This hike contains 94 steps. (The other possible hikes you could have taken were 90, 86, 82, 82, and 74 steps long.)

   Find the longest hike you can take through the hiking trails listed on your map. How many steps long is the longest hike?
*)

(* https://www.youtube.com/watch?v=NTLYL7Mg2jU *)

open Utils

(* BFS, but can visit again if steps taken is longer *)
type dir = Up | Down | Left | Right
type tile = Path | Forest | Slope of dir

module T = struct
  type t = int * int

  let compare = compare
end

module TM = Map.Make (T)
module TS = Set.Make (T)

let ( ++ ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let dirs = [ Up; Down; Left; Right ]

let char_to_dir = function
  | '>' -> Right
  | '<' -> Left
  | '^' -> Up
  | 'v' -> Down
  | _ -> raise @@ Invalid_argument "Cannot parse char to dir"

let char_to_tile = function
  | '.' -> Path
  | '#' -> Forest
  | ('>' | '<' | '^' | 'v') as c -> Slope (char_to_dir c)
  | _ -> raise @@ Invalid_argument "Cannot parse char to tile"

let dir_offset = function
  | Up -> (-1, 0)
  | Down -> (1, 0)
  | Left -> (0, -1)
  | Right -> (0, 1)

let positions lines =
  List.fold_left
    (fun (r, racc) line ->
      ( r + 1,
        snd
        @@ List.fold_left
             (fun (c, lacc) v -> (c + 1, TM.add (r, c) (char_to_tile v) lacc))
             (0, racc) line ))
    (0, TM.empty) lines
  |> snd

let num_connections tiles (r, c) =
  List.filter
    (fun d ->
      match TM.find_opt (dir_offset d ++ (r, c)) tiles with
      | None -> false
      | Some t -> t <> Forest)
    dirs
  |> List.length

let start_and_finish tiles =
  ( TM.fold
      (fun (r, c) t (racc, cacc) ->
        if r > 0 then (racc, cacc) else if t = Path then (r, c) else (racc, cacc))
      tiles (0, 0),
    TM.fold
      (fun (r, c) t (racc, cacc) ->
        if r > racc then (r, c)
        else if r < racc then (racc, cacc)
        else if t = Path then (r, c)
        else (racc, cacc))
      tiles (0, 0) )

let points_of_interest tiles =
  let start, finish = start_and_finish tiles in
  let rec aux pts = function
    | [] -> pts
    | ((r, c), til) :: t ->
        aux
          (if til <> Forest && num_connections tiles (r, c) >= 3 then
             (r, c) :: pts
           else pts)
          t
  in
  aux [ start; finish ] @@ TM.bindings tiles

let eligible_dirs = function Forest -> [] | Slope d -> [ d ] | Path -> dirs

let build_graph tiles =
  let pts = points_of_interest tiles in
  let rec aux seen graph (sr, sc) = function
    | [] -> graph
    | (dist, r, c) :: t ->
        if dist <> 0 && List.mem (r, c) pts then
          let g' =
            TM.update (sr, sc)
              (function
                | None -> failwith "impossible"
                | Some m -> Some (TM.add (r, c) dist m))
              graph
          in
          aux (TS.add (r, c) seen) g' (sr, sc) t
        else
          let neighbors =
            List.map
              (fun d -> (r, c) ++ dir_offset d)
              (eligible_dirs @@ TM.find (r, c) tiles)
            |> List.filter (fun p ->
                   match TM.find_opt p tiles with
                   | None -> false
                   | Some til -> til <> Forest && (not @@ TS.mem p seen))
          in
          let seen' =
            List.fold_left (fun acc p -> TS.add p acc) seen neighbors
          in
          let stack =
            List.fold_left
              (fun acc (r, c) -> (dist + 1, r, c) :: acc)
              t neighbors
          in
          aux seen' graph (sr, sc) stack
  in
  let g = TM.of_list (List.map (fun p -> (p, TM.empty)) pts) in
  List.fold_left
    (fun gacc (r, c) -> aux (TS.of_list [ (r, c) ]) gacc (r, c) [ (0, r, c) ])
    g pts

let dfs tiles graph =
  let start, finish = start_and_finish tiles in
  let rec aux seen pt =
    if pt = finish then 0.
    else
      let seen' = TS.add pt seen in
      List.fold_left
        (fun acc (neighbor, dist) ->
          if TS.mem neighbor seen then acc
          else max acc (aux seen' neighbor +. float_of_int dist))
        (-1. *. infinity)
        (TM.find pt graph |> TM.bindings)
  in
  aux TS.empty start |> int_of_float

let part_one file =
  let lines = file_lines file |> List.map char_list_of_string in
  let tiles = positions lines in
  let graph = build_graph tiles in
  dfs tiles graph

(*
   --- Part Two ---
   As you reach the trailhead, you realize that the ground isn't as slippery as you expected; you'll have no problem climbing up the steep slopes.

   Now, treat all slopes as if they were normal paths (.). You still want to make sure you have the most scenic hike possible, so continue to ensure that you never step onto the same tile twice. What is the longest hike you can take?

   In the example above, this increases the longest hike to 154 steps:

   #S#####################
   #OOOOOOO#########OOO###
   #######O#########O#O###
   ###OOOOO#.>OOO###O#O###
   ###O#####.#O#O###O#O###
   ###O>...#.#O#OOOOO#OOO#
   ###O###.#.#O#########O#
   ###OOO#.#.#OOOOOOO#OOO#
   #####O#.#.#######O#O###
   #OOOOO#.#.#OOOOOOO#OOO#
   #O#####.#.#O#########O#
   #O#OOO#...#OOO###...>O#
   #O#O#O#######O###.###O#
   #OOO#O>.#...>O>.#.###O#
   #####O#.#.###O#.#.###O#
   #OOOOO#...#OOO#.#.#OOO#
   #O#########O###.#.#O###
   #OOO###OOO#OOO#...#O###
   ###O###O#O###O#####O###
   #OOO#OOO#O#OOO>.#.>O###
   #O###O###O#O###.#.#O###
   #OOOOO###OOO###...#OOO#
   #####################O#
   Find the longest hike you can take through the surprisingly dry hiking trails listed on your map. How many steps long is the longest hike?
*)

let part_two file =
  let lines = file_lines file |> List.map char_list_of_string in
  let tiles =
    positions lines |> TM.map (fun t -> if t = Forest then Forest else Path)
  in
  let graph = build_graph tiles in
  dfs tiles graph
