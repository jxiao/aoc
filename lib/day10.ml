open Utils

(*
   --- Day 10: Pipe Maze ---
   You use the hang glider to ride the hot air from Desert Island all the way up to the floating metal island. This island is surprisingly cold and there definitely aren't any thermals to glide on, so you leave your hang glider behind.

   You wander around for a while, but you don't find any people or animals. However, you do occasionally find signposts labeled "Hot Springs" pointing in a seemingly consistent direction; maybe you can find someone at the hot springs and ask them where the desert-machine parts are made.

   The landscape here is alien; even the flowers and trees are made of metal. As you stop to admire some metal grass, you notice something metallic scurry away in your peripheral vision and jump into a big pipe! It didn't look like any animal you've ever seen; if you want a better look, you'll need to get ahead of it.

   Scanning the area, you discover that the entire field you're standing on is densely packed with pipes; it was hard to tell at first because they're the same metallic silver color as the "ground". You make a quick sketch of all of the surface pipes you can see (your puzzle input).

   The pipes are arranged in a two-dimensional grid of tiles:

   | is a vertical pipe connecting north and south.
   - is a horizontal pipe connecting east and west.
   L is a 90-degree bend connecting north and east.
   J is a 90-degree bend connecting north and west.
   7 is a 90-degree bend connecting south and west.
   F is a 90-degree bend connecting south and east.
   . is ground; there is no pipe in this tile.
   S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
   Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

   For example, here is a square loop of pipe:

   .....
   .F-7.
   .|.|.
   .L-J.
   .....
   If the animal had entered this loop in the northwest corner, the sketch would instead look like this:

   .....
   .S-7.
   .|.|.
   .L-J.
   .....
   In the above diagram, the S tile is still a 90-degree F bend: you can tell because of how the adjacent pipes connect to it.

   Unfortunately, there are also many pipes that aren't connected to the loop! This sketch shows the same loop as above:

   -L|F7
   7S-7|
   L|7||
   -L-J|
   L|-JF
   In the above diagram, you can still figure out which pipes form the main loop: they're the ones connected to S, pipes those pipes connect to, pipes those pipes connect to, and so on. Every pipe in the main loop connects to its two neighbors (including S, which will have exactly two pipes connecting to it, and which is assumed to connect back to those two pipes).

   Here is a sketch that contains a slightly more complex main loop:

   ..F7.
   .FJ|.
   SJ.L7
   |F--J
   LJ...
   Here's the same example sketch with the extra, non-main-loop pipe tiles also shown:

   7-F7-
   .FJ|7
   SJLL7
   |F--J
   LJ.LJ
   If you want to get out ahead of the animal, you should find the tile in the loop that is farthest from the starting position. Because the animal is in the pipe, it doesn't make sense to measure this by direct distance. Instead, you need to find the tile that would take the longest number of steps along the loop to reach from the starting point - regardless of which way around the loop the animal went.

   In the first example with the square loop:

   .....
   .S-7.
   .|.|.
   .L-J.
   .....
   You can count the distance each tile in the loop is from the starting point like this:

   .....
   .012.
   .1.3.
   .234.
   .....
   In this example, the farthest point from the start is 4 steps away.

   Here's the more complex loop again:

   ..F7.
   .FJ|.
   SJ.L7
   |F--J
   LJ...
   Here are the distances for each tile on that loop:

   ..45.
   .236.
   01.78
   14567
   23...
   Find the single giant loop starting at S. How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?
*)
type dir = N | S | W | E

let dir_to_diff = function
  | N -> (-1, 0)
  | S -> (1, 0)
  | W -> (0, -1)
  | E -> (0, 1)

let opp_dir = function N -> S | S -> N | W -> E | E -> W

type pipe = NS | WE | NE | NW | SW | SE

let pipe_to_dirs = function
  | NS -> (N, S)
  | WE -> (W, E)
  | NE -> (N, E)
  | NW -> (N, W)
  | SW -> (S, W)
  | SE -> (S, E)

type tile = Ground | Pipe of pipe | Start of pipe option

module Pos = struct
  type t = int * int

  let compare = compare
end

module TSet = Set.Make (Pos)

let tiles = [ NS; WE; NE; NW; SW; SE ]

let char_to_tile = function
  | '|' -> Pipe NS
  | '-' -> Pipe WE
  | 'L' -> Pipe NE
  | 'J' -> Pipe NW
  | '7' -> Pipe SW
  | 'F' -> Pipe SE
  | '.' -> Ground
  | 'S' -> Start None
  | c ->
      Invalid_argument (Printf.sprintf "Invalid char \"%c\" found." c) |> raise

(* Computes mapping from position to pipe along with the starting position *)
let pipes lines =
  let rec parse mapping start r = function
    | [] -> (mapping, start)
    | h :: t ->
        let start', _ =
          char_list_of_string h
          |> List.fold_left
               (fun (st, i) c ->
                 match char_to_tile c with
                 | Ground -> (st, i + 1)
                 | Start _ as tile ->
                     Hashtbl.add mapping (r, i) tile;
                     (Some (r, i), i + 1)
                 | tile ->
                     Hashtbl.add mapping (r, i) tile;
                     (st, i + 1))
               (start, 0)
        in
        parse mapping start' (r + 1) t
  in
  parse (Hashtbl.create (List.length lines * List.length lines)) None 0 lines

let dirs = [ N; S; W; E ]

let can_enter_pipe_from (pipe : pipe) dir =
  let l, r = pipe_to_dirs pipe in
  dir = l || dir = r

let furthest_cycle pipes start =
  let rec dfs s orientation =
    match s with
    | [] -> None
    | ((r, c), steps, None) :: t ->
        let lft, rht = pipe_to_dirs orientation in
        let s' =
          List.filter_map
            (fun d ->
              if d = lft || d = rht then
                let dr, dc = dir_to_diff d in
                let pos' = (r + dr, c + dc) in
                let opp = opp_dir d in
                match Hashtbl.find_opt pipes pos' with
                | Some (Pipe p) ->
                    if can_enter_pipe_from p opp then
                      Some (pos', steps + 1, Some opp)
                    else None
                (* Following case shouldn't match onto Start *)
                | _ -> None
              else None)
            dirs
          @ t
        in
        dfs s' orientation
    | (((r, c) as pos), steps, Some pre_dir) :: t -> (
        match Hashtbl.find_opt pipes pos with
        | Some (Start _) -> Some (steps / 2)
        | Some (Pipe p) ->
            let e1, e2 = pipe_to_dirs p in
            let s' =
              List.filter_map
                (fun d ->
                  if (d = e1 || d = e2) && d <> pre_dir then
                    let dr, dc = dir_to_diff d in
                    let pos' = (r + dr, c + dc) in
                    let opp = opp_dir d in
                    match Hashtbl.find_opt pipes pos' with
                    | Some (Pipe p') ->
                        if can_enter_pipe_from p' opp then
                          Some (pos', steps + 1, Some opp)
                        else None
                    | Some (Start _) ->
                        if can_enter_pipe_from orientation opp then
                          Some (pos', steps + 1, Some opp)
                        else None
                    | _ -> None
                  else None)
                dirs
              @ t
            in
            dfs s' orientation
        | _ -> dfs t orientation)
  in
  List.filter_map (dfs [ (start, 0, None) ]) tiles
  |> List.fold_left max Int.min_int

let part_one file =
  let lines = file_lines file in
  match pipes lines with
  | _, None -> Invalid_argument "No start found." |> raise
  | ps, Some start -> furthest_cycle ps start
