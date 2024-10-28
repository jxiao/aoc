(*
   --- Day 16: The Floor Will Be Lava ---
   With the beam of light completely focused somewhere, the reindeer leads you deeper still into the Lava Production Facility. At some point, you realize that the steel facility walls have been replaced with cave, and the doorways are just cave, and the floor is cave, and you're pretty sure this is actually just a giant cave.

   Finally, as you approach what must be the heart of the mountain, you see a bright light in a cavern up ahead. There, you discover that the beam of light you so carefully focused is emerging from the cavern wall closest to the facility and pouring all of its energy into a contraption on the opposite side.

   Upon closer inspection, the contraption appears to be a flat, two-dimensional square grid containing empty space (.), mirrors (/ and \), and splitters (| and -).

   The contraption is aligned so that most of the beam bounces around the grid, but each tile on the grid converts some of the beam's light into heat to melt the rock in the cavern.

   You note the layout of the contraption (your puzzle input). For example:

   .|...\....
   |.-.\.....
   .....|-...
   ........|.
   ..........
   .........\
   ..../.\\..
   .-.-/..|..
   .|....-|.\
   ..//.|....
   The beam enters in the top-left corner from the left and heading to the right. Then, its behavior depends on what it encounters as it moves:

   If the beam encounters empty space (.), it continues in the same direction.
   If the beam encounters a mirror (/ or \), the beam is reflected 90 degrees depending on the angle of the mirror. For instance, a rightward-moving beam that encounters a / mirror would continue upward in the mirror's column, while a rightward-moving beam that encounters a \ mirror would continue downward from the mirror's column.
   If the beam encounters the pointy end of a splitter (| or -), the beam passes through the splitter as if the splitter were empty space. For instance, a rightward-moving beam that encounters a - splitter would continue in the same direction.
   If the beam encounters the flat side of a splitter (| or -), the beam is split into two beams going in each of the two directions the splitter's pointy ends are pointing. For instance, a rightward-moving beam that encounters a | splitter would split into two beams: one that continues upward from the splitter's column and one that continues downward from the splitter's column.
   Beams do not interact with other beams; a tile can have many beams passing through it at the same time. A tile is energized if that tile has at least one beam pass through it, reflect in it, or split in it.

   In the above example, here is how the beam of light bounces around the contraption:

   >|<<<\....
   |v-.\^....
   .v...|->>>
   .v...v^.|.
   .v...v^...
   .v...v^..\
   .v../2\\..
   <->-/vv|..
   .|<<<2-|.\
   .v//.|.v..
   Beams are only shown on empty tiles; arrows indicate the direction of the beams. If a tile contains beams moving in multiple directions, the number of distinct directions is shown instead. Here is the same diagram but instead only showing whether a tile is energized (#) or not (.):

   ######....
   .#...#....
   .#...#####
   .#...##...
   .#...##...
   .#...##...
   .#..####..
   ########..
   .#######..
   .#...#.#..
   Ultimately, in this example, 46 tiles become energized.

   The light isn't energizing enough tiles to produce lava; to debug the contraption, you need to start by analyzing the current situation. With the beam starting in the top-left heading right, how many tiles end up being energized?
*)

open Utils

type dir = Up | Down | Left | Right
type tile = Empty | Vertical | Horizontal | RightSlant | LeftSlant

let char_to_tile = function
  | '.' -> Empty
  | '|' -> Vertical
  | '-' -> Horizontal
  | '\\' -> RightSlant
  | '/' -> LeftSlant
  | _ -> Invalid_argument "Unknown character" |> raise

let grid_pos lines =
  let positions = Hashtbl.create 1000 in
  List.iteri
    (fun r line ->
      List.iteri (fun c v -> Hashtbl.add positions (r, c) (char_to_tile v)) line)
    lines;
  positions

let dir_to_diff = function
  | Up -> (-1, 0)
  | Down -> (1, 0)
  | Left -> (0, -1)
  | Right -> (0, 1)

let add (r, c) (dr, dc) = (r + dr, c + dc)
let in_bounds (r, c) (nrows, ncols) = r >= 0 && c >= 0 && r < nrows && c < ncols

let continue (r, c) dir =
  let r', c' = dir_to_diff dir |> add (r, c) in
  [ (r', c', dir) ]

let move_dir (r, c) new_dir (nrows, ncols) =
  let next =
    let r', c' = dir_to_diff new_dir |> add (r, c) in
    if in_bounds (r', c') (nrows, ncols) then Some (r', c', new_dir) else None
  in
  List.filter_map Fun.id [ next ]

let nexts positions (r, c) dir (nrows, ncols) =
  match (Hashtbl.find positions (r, c), dir) with
  | Empty, _
  | Horizontal, Left
  | Horizontal, Right
  | Vertical, Up
  | Vertical, Down ->
      move_dir (r, c) dir (nrows, ncols) @ move_dir (r, c) dir (nrows, ncols)
  | Horizontal, _ ->
      move_dir (r, c) Left (nrows, ncols) @ move_dir (r, c) Right (nrows, ncols)
  | Vertical, _ ->
      move_dir (r, c) Up (nrows, ncols) @ move_dir (r, c) Down (nrows, ncols)
  | RightSlant, Right | LeftSlant, Left -> move_dir (r, c) Down (nrows, ncols)
  | RightSlant, Left | LeftSlant, Right -> move_dir (r, c) Up (nrows, ncols)
  | RightSlant, Up | LeftSlant, Down -> move_dir (r, c) Left (nrows, ncols)
  | RightSlant, Down | LeftSlant, Up -> move_dir (r, c) Right (nrows, ncols)

let dfs (r, c, dir) nrows ncols positions =
  let seen = Hashtbl.create 1000 in
  let stack = Stack.create () in
  Stack.push (r, c, dir) stack;
  let rec aux () =
    match Stack.pop_opt stack with
    | None -> ()
    | Some (r, c, dir) ->
        if Hashtbl.mem seen (r, c, dir) then ()
        else (
          Hashtbl.add seen (r, c, dir) ();
          let next = nexts positions (r, c) dir (nrows, ncols) in
          List.iter (fun tup -> Stack.push tup stack) next);
        if Stack.is_empty stack |> not then aux ()
  in
  aux ();
  Hashtbl.to_seq_keys seen
  |> Seq.map (fun (r, c, _) -> (r, c))
  |> List.of_seq |> List.sort_uniq compare |> List.length

let max_energy starts lines positions =
  let nrows, ncols = (List.length lines, List.hd lines |> List.length) in
  List.fold_left
    (fun acc start -> dfs start nrows ncols positions |> max acc)
    0 starts

let part_one file =
  let lines = file_lines file |> List.map char_list_of_string in
  let positions = grid_pos lines in
  max_energy [ (0, 0, Right) ] lines positions

(*
   --- Part Two ---
   As you try to work out what might be wrong, the reindeer tugs on your shirt and leads you to a nearby control panel. There, a collection of buttons lets you align the contraption so that the beam enters from any edge tile and heading away from that edge. (You can choose either of two directions for the beam if it starts on a corner; for instance, if the beam starts in the bottom-right corner, it can start heading either left or upward.)

   So, the beam could start on any tile in the top row (heading downward), any tile in the bottom row (heading upward), any tile in the leftmost column (heading right), or any tile in the rightmost column (heading left). To produce lava, you need to find the configuration that energizes as many tiles as possible.

   In the above example, this can be achieved by starting the beam in the fourth tile from the left in the top row:

   .|<2<\....
   |v-v\^....
   .v.v.|->>>
   .v.v.v^.|.
   .v.v.v^...
   .v.v.v^..\
   .v.v/2\\..
   <-2-/vv|..
   .|<<<2-|.\
   .v//.|.v..
   Using this configuration, 51 tiles are energized:

   .#####....
   .#.#.#....
   .#.#.#####
   .#.#.##...
   .#.#.##...
   .#.#.##...
   .#.#####..
   ########..
   .#######..
   .#...#.#..
   Find the initial beam configuration that energizes the largest number of tiles; how many tiles are energized in that configuration?
*)

let part_two file =
  let lines = file_lines file |> List.map char_list_of_string in
  let positions = grid_pos lines in
  let nrows, ncols = (List.length lines, List.hd lines |> List.length) in
  let rows, cols = (List.init nrows Fun.id, List.init ncols Fun.id) in
  let top = List.map (fun c -> (0, c, Down)) cols in
  let bottom = List.map (fun c -> (nrows - 1, c, Up)) cols in
  let left = List.map (fun r -> (r, 0, Right)) rows in
  let right = List.map (fun r -> (r, ncols - 1, Left)) rows in
  max_energy (top @ bottom @ left @ right) lines positions
