(*
   --- Day 21: Step Counter ---
   You manage to catch the airship right as it's dropping someone else off on their all-expenses-paid trip to Desert Island! It even helpfully drops you off near the gardener and his massive farm.

   "You got the sand flowing again! Great work! Now we just need to wait until we have enough sand to filter the water for Snow Island and we'll have snow again in no time."

   While you wait, one of the Elves that works with the gardener heard how good you are at solving problems and would like your help. He needs to get his steps in for the day, and so he'd like to know which garden plots he can reach with exactly his remaining 64 steps.

   He gives you an up-to-date map (your puzzle input) of his starting position (S), garden plots (.), and rocks (#). For example:

   ...........
   .....###.#.
   .###.##..#.
   ..#.#...#..
   ....#.#....
   .##..S####.
   .##..#...#.
   .......##..
   .##.#.####.
   .##..##.##.
   ...........
   The Elf starts at the starting position (S) which also counts as a garden plot. Then, he can take one step north, south, east, or west, but only onto tiles that are garden plots. This would allow him to reach any of the tiles marked O:

   ...........
   .....###.#.
   .###.##..#.
   ..#.#...#..
   ....#O#....
   .##.OS####.
   .##..#...#.
   .......##..
   .##.#.####.
   .##..##.##.
   ...........
   Then, he takes a second step. Since at this point he could be at either tile marked O, his second step would allow him to reach any garden plot that is one step north, south, east, or west of any tile that he could have reached after the first step:

   ...........
   .....###.#.
   .###.##..#.
   ..#.#O..#..
   ....#.#....
   .##O.O####.
   .##.O#...#.
   .......##..
   .##.#.####.
   .##..##.##.
   ...........
   After two steps, he could be at any of the tiles marked O above, including the starting position (either by going north-then-south or by going west-then-east).

   A single third step leads to even more possibilities:

   ...........
   .....###.#.
   .###.##..#.
   ..#.#.O.#..
   ...O#O#....
   .##.OS####.
   .##O.#...#.
   ....O..##..
   .##.#.####.
   .##..##.##.
   ...........
   He will continue like this until his steps for the day have been exhausted. After a total of 6 steps, he could reach any of the garden plots marked O:

   ...........
   .....###.#.
   .###.##.O#.
   .O#O#O.O#..
   O.O.#.#.O..
   .##O.O####.
   .##.O#O..#.
   .O.O.O.##..
   .##.#.####.
   .##O.##.##.
   ...........
   In this example, if the Elf's goal was to get exactly 6 more steps today, he could use them to reach any of 16 garden plots.

   However, the Elf actually needs to get 64 steps today, and the map he's handed you is much larger than the example map.

   Starting from the garden plot marked S on your map, how many garden plots could the Elf reach in exactly 64 steps?
*)

open Utils

(* BFS, but do not keep track of visited *)

module Tup = struct
  type t = int * int

  let compare = compare
end

module M = Map.Make (Tup)
module S = Set.Make (Tup)

type plot = Start | Garden | Rock
type tile = { plot : plot; r : int; c : int }

let adjs (r, c) = [ (r + 1, c); (r - 1, c); (r, c + 1); (r, c - 1) ]

let char_to_plot = function
  | '.' -> Garden
  | '#' -> Rock
  | 'S' -> Start
  | _ -> raise @@ Invalid_argument "Cannot decode char to plot"

let rec bfs pos curr next =
  match curr with
  | [] -> next
  | (r, c) :: t ->
      let valid_neighbors =
        adjs (r, c)
        |> List.filter_map (fun (r', c') ->
               match M.find_opt (r', c') pos with
               | None -> None
               | Some { plot; _ } -> if plot = Rock then None else Some (r', c'))
      in
      bfs pos t (List.fold_left (fun acc p -> S.add p acc) next valid_neighbors)

let positions lines =
  List.fold_left
    (fun (r, racc) line ->
      ( r + 1,
        snd
        @@ List.fold_left
             (fun (c, lacc) v ->
               (c + 1, M.add (r, c) { plot = char_to_plot v; r; c } lacc))
             (0, racc) line ))
    (0, M.empty) lines
  |> snd

let part_one file =
  let lines = file_lines file |> List.map char_list_of_string in
  let pos = positions lines in
  let r, c =
    M.bindings pos
    |> List.filter (fun (_, { plot; _ }) -> plot = Start)
    |> List.hd |> fst
  in
  let res =
    List.fold_left
      (fun acc _ -> bfs pos (S.to_list acc) S.empty)
      (S.of_list [ (r, c) ])
      (List.init 64 Fun.id)
  in
  S.cardinal res

(*
   --- Part Two ---
   The Elf seems confused by your answer until he realizes his mistake: he was reading from a list of his favorite numbers that are both perfect squares and perfect cubes, not his step counter.

   The actual number of steps he needs to get today is exactly 26501365.

   He also points out that the garden plots and rocks are set up so that the map repeats infinitely in every direction.

   So, if you were to look one additional map-width or map-height out from the edge of the example map above, you would find that it keeps repeating:

   .................................
   .....###.#......###.#......###.#.
   .###.##..#..###.##..#..###.##..#.
   ..#.#...#....#.#...#....#.#...#..
   ....#.#........#.#........#.#....
   .##...####..##...####..##...####.
   .##..#...#..##..#...#..##..#...#.
   .......##.........##.........##..
   .##.#.####..##.#.####..##.#.####.
   .##..##.##..##..##.##..##..##.##.
   .................................
   .................................
   .....###.#......###.#......###.#.
   .###.##..#..###.##..#..###.##..#.
   ..#.#...#....#.#...#....#.#...#..
   ....#.#........#.#........#.#....
   .##...####..##..S####..##...####.
   .##..#...#..##..#...#..##..#...#.
   .......##.........##.........##..
   .##.#.####..##.#.####..##.#.####.
   .##..##.##..##..##.##..##..##.##.
   .................................
   .................................
   .....###.#......###.#......###.#.
   .###.##..#..###.##..#..###.##..#.
   ..#.#...#....#.#...#....#.#...#..
   ....#.#........#.#........#.#....
   .##...####..##...####..##...####.
   .##..#...#..##..#...#..##..#...#.
   .......##.........##.........##..
   .##.#.####..##.#.####..##.#.####.
   .##..##.##..##..##.##..##..##.##.
   .................................
   This is just a tiny three-map-by-three-map slice of the inexplicably-infinite farm layout; garden plots and rocks repeat as far as you can see. The Elf still starts on the one middle tile marked S, though - every other repeated S is replaced with a normal garden plot (.).

   Here are the number of reachable garden plots in this new infinite version of the example map for different numbers of steps:

   In exactly 6 steps, he can still reach 16 garden plots.
   In exactly 10 steps, he can reach any of 50 garden plots.
   In exactly 50 steps, he can reach 1594 garden plots.
   In exactly 100 steps, he can reach 6536 garden plots.
   In exactly 500 steps, he can reach 167004 garden plots.
   In exactly 1000 steps, he can reach 668697 garden plots.
   In exactly 5000 steps, he can reach 16733044 garden plots.
   However, the step count the Elf needs is much larger! Starting from the garden plot marked S on your infinite map, how many garden plots could the Elf reach in exactly 26501365 steps?
*)

(* Savior: https://advent-of-code.xavd.id/writeups/2023/day/21/ *)

let rec bfs_with_visited (nr, nc) pos visited = function
  | [] -> visited
  | (d, (r, c)) :: t ->
      if M.mem (r, c) visited then bfs_with_visited (nr, nc) pos visited t
      else
        let valid_neighbors =
          adjs (r, c)
          |> List.filter_map (fun (r', c') ->
                 if
                   r' < 0 || c' < 0 || r' >= nr || c' >= nc
                   || M.mem (r', c') visited
                   || (M.find (r', c') pos).plot = Rock
                 then None
                 else Some (d + 1, (r', c')))
        in
        let visited' = M.add (r, c) d visited in
        bfs_with_visited (nr, nc) pos visited' (t @ valid_neighbors)

let ( ** ) x y = float_of_int x ** float_of_int y |> int_of_float

let part_two file =
  let lines = file_lines file |> List.map char_list_of_string in
  let pos = positions lines in
  let nr, nc = (List.length lines, List.hd lines |> List.length) in
  let dist_to_edge = nr / 2 in
  let n = (26501365 - dist_to_edge) / nr in
  let n_odd_tiles, n_even_tiles = ((n + 1) ** 2, n ** 2) in
  let r, c =
    M.bindings pos
    |> List.filter (fun (_, { plot; _ }) -> plot = Start)
    |> List.hd |> fst
  in
  let visited = bfs_with_visited (nr, nc) pos M.empty [ (0, (r, c)) ] in
  let odd_corners =
    M.bindings visited
    |> List.filter (fun (_, d) -> d > dist_to_edge && d mod 2 = 1)
    |> List.length
  in
  let even_corners =
    M.bindings visited
    |> List.filter (fun (_, d) -> d > dist_to_edge && d mod 2 = 0)
    |> List.length
  in
  let odds =
    M.bindings visited |> List.filter (fun (_, d) -> d mod 2 = 1) |> List.length
  in
  let evens =
    M.bindings visited |> List.filter (fun (_, d) -> d mod 2 = 0) |> List.length
  in
  (n_odd_tiles * odds) + (n_even_tiles * evens)
  - ((n + 1) * odd_corners)
  + (n * even_corners)

(*
   Post-game recap: not a fan of this problem.
*)
