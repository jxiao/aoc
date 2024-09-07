open Utils
(*
   --- Day 11: Cosmic Expansion ---
   You continue following signs for "Hot Springs" and eventually come across an observatory. The Elf within turns out to be a researcher studying cosmic expansion using the giant telescope here.

   He doesn't know anything about the missing machine parts; he's only visiting for this research project. However, he confirms that the hot springs are the next-closest area likely to have people; he'll even take you straight there once he's done with today's observation analysis.

   Maybe you can help him with the analysis to speed things up?

   The researcher has collected a bunch of data and compiled the data into a single giant image (your puzzle input). The image includes empty space (.) and galaxies (#). For example:

   ...#......
   .......#..
   #.........
   ..........
   ......#...
   .#........
   .........#
   ..........
   .......#..
   #...#.....
   The researcher is trying to figure out the sum of the lengths of the shortest path between every pair of galaxies. However, there's a catch: the universe expanded in the time it took the light from those galaxies to reach the observatory.

   Due to something involving gravitational effects, only some space expands. In fact, the result is that any rows or columns that contain no galaxies should all actually be twice as big.

   In the above example, three columns and two rows contain no galaxies:

      v  v  v
    ...#......
    .......#..
    #.........
   >..........<
    ......#...
    .#........
    .........#
   >..........<
    .......#..
    #...#.....
      ^  ^  ^
   These rows and columns need to be twice as big; the result of cosmic expansion therefore looks like this:

   ....#........
   .........#...
   #............
   .............
   .............
   ........#....
   .#...........
   ............#
   .............
   .............
   .........#...
   #....#.......
   Equipped with this expanded universe, the shortest path between every pair of galaxies can be found. It can help to assign every galaxy a unique number:

   ....1........
   .........2...
   3............
   .............
   .............
   ........4....
   .5...........
   ............6
   .............
   .............
   .........7...
   8....9.......
   In these 9 galaxies, there are 36 pairs. Only count each pair once; order within the pair doesn't matter. For each pair, find any shortest path between the two galaxies using only steps that move up, down, left, or right exactly one . or # at a time. (The shortest path between two galaxies is allowed to pass through another galaxy.)

   For example, here is one of the shortest paths between galaxies 5 and 9:

   ....1........
   .........2...
   3............
   .............
   .............
   ........4....
   .5...........
   .##.........6
   ..##.........
   ...##........
   ....##...7...
   8....9.......
   This path has length 9 because it takes a minimum of nine steps to get from galaxy 5 to galaxy 9 (the eight locations marked # plus the step onto galaxy 9 itself). Here are some other example shortest path lengths:

   Between galaxy 1 and galaxy 7: 15
   Between galaxy 3 and galaxy 6: 17
   Between galaxy 8 and galaxy 9: 5
   In this example, after expanding the universe, the sum of the shortest path between all 36 pairs of galaxies is 374.

   Expand the universe, then find the length of the shortest path between every pair of galaxies. What is the sum of these lengths?
*)
type coordinate = int * int

module C = struct
  type t = coordinate

  let compare = compare
end

module CSet = Set.Make (C)
module ISet = Set.Make (Int)

let range n =
  let rec range_rec i acc =
    if i < 0 then Invalid_argument "Negative #" |> raise
    else if i = 0 then acc
    else range_rec (i - 1) (i :: acc)
  in
  range_rec n []

let get_galaxies lines =
  let rec read_rec r acc = function
    | [] -> acc
    | h :: t ->
        let row_galaxies, _ =
          char_list_of_string h
          |> List.fold_left
               (fun (accl, i) c ->
                 if c = '#' then ((r, i) :: accl, i + 1) else (accl, i + 1))
               ([], 0)
        in
        read_rec (r + 1) (row_galaxies @ acc) t
  in
  read_rec 0 [] lines |> CSet.of_list

let galaxies_info lines =
  let galaxies = get_galaxies lines in
  let rows = List.length lines |> range |> ISet.of_list in
  let cols = List.hd lines |> String.length |> range |> ISet.of_list in
  let empty_rows, empty_cols =
    CSet.fold
      (fun (r, c) (racc, cacc) -> (ISet.remove r racc, ISet.remove c cacc))
      galaxies (rows, cols)
  in
  (galaxies, (empty_rows, empty_cols))

let count_between v1 v2 set =
  ISet.filter_map
    (function i when i > min v1 v2 && i < max v1 v2 -> Some i | _ -> None)
    set
  |> ISet.cardinal

let dist (row_exs, col_exs) multiplier (r1, c1) (r2, c2) =
  let r' = count_between r1 r2 row_exs in
  let c' = count_between c1 c2 col_exs in
  abs (r1 - r2)
  + abs (c1 - c2)
  + (r' * (multiplier - 1))
  + (c' * (multiplier - 1))

let apsp lines multiplier =
  let galaxies, no_galaxies = galaxies_info lines in
  let rec apsp_rec acc = function
    | [] -> acc
    | h :: t ->
        let dists_from_curr =
          List.map (dist no_galaxies multiplier h) t |> sum
        in
        apsp_rec (dists_from_curr + acc) t
  in
  CSet.to_list galaxies |> apsp_rec 0

let part_one file =
  let lines = file_lines file in
  apsp lines 2

(*
   --- Part Two ---
   The galaxies are much older (and thus much farther apart) than the researcher initially estimated.

   Now, instead of the expansion you did before, make each empty row or column one million times larger. That is, each empty row should be replaced with 1000000 empty rows, and each empty column should be replaced with 1000000 empty columns.

   (In the example above, if each empty row or column were merely 10 times larger, the sum of the shortest paths between every pair of galaxies would be 1030. If each empty row or column were merely 100 times larger, the sum of the shortest paths between every pair of galaxies would be 8410. However, your universe will need to expand far beyond these values.)

   Starting with the same initial image, expand the universe according to these new rules, then find the length of the shortest path between every pair of galaxies. What is the sum of these lengths?
*)

let part_two file =
  let lines = file_lines file in
  apsp lines 1000000
