(*
   --- Day 18: Lavaduct Lagoon ---
   Thanks to your efforts, the machine parts factory is one of the first factories up and running since the lavafall came back. However, to catch up with the large backlog of parts requests, the factory will also need a large supply of lava for a while; the Elves have already started creating a large lagoon nearby for this purpose.

   However, they aren't sure the lagoon will be big enough; they've asked you to take a look at the dig plan (your puzzle input). For example:

   R 6 (#70c710)
   D 5 (#0dc571)
   L 2 (#5713f0)
   D 2 (#d2c081)
   R 2 (#59c680)
   D 2 (#411b91)
   L 5 (#8ceee2)
   U 2 (#caa173)
   L 1 (#1b58a2)
   U 2 (#caa171)
   R 2 (#7807d2)
   U 3 (#a77fa3)
   L 2 (#015232)
   U 2 (#7a21e3)
   The digger starts in a 1 meter cube hole in the ground. They then dig the specified number of meters up (U), down (D), left (L), or right (R), clearing full 1 meter cubes as they go. The directions are given as seen from above, so if "up" were north, then "right" would be east, and so on. Each trench is also listed with the color that the edge of the trench should be painted as an RGB hexadecimal color code.

   When viewed from above, the above example dig plan would result in the following loop of trench (#) having been dug out from otherwise ground-level terrain (.):

   #######
   #.....#
   ###...#
   ..#...#
   ..#...#
   ###.###
   #...#..
   ##..###
   .#....#
   .######
   At this point, the trench could contain 38 cubic meters of lava. However, this is just the edge of the lagoon; the next step is to dig out the interior so that it is one meter deep as well:

   #######
   #######
   #######
   ..#####
   ..#####
   #######
   #####..
   #######
   .######
   .######
   Now, the lagoon can contain a much more respectable 62 cubic meters of lava. While the interior is dug out, the edges are also painted according to the color codes in the dig plan.

   The Elves are concerned the lagoon won't be large enough; if they follow their dig plan, how many cubic meters of lava could it hold?
*)
open Utils

type direction = R | L | U | D

let str_to_dir = function
  | "R" | "0" -> R
  | "L" | "2" -> L
  | "U" | "3" -> U
  | "D" | "1" -> D
  | s ->
      raise
      @@ Invalid_argument
           (Printf.sprintf "Unknown string. Cannot map to direction: %s" s)

let dir_offset = function
  | R -> (0, 1)
  | L -> (0, -1)
  | U -> (-1, 0)
  | D -> (1, 0)

let ( ++ ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let ( *** ) mag (x, y) = (mag * x, mag * y)

type dig = { dir : direction; mag : int; color : string }

let extract_color s = String.sub s 2 6

let parse line =
  match String.split_on_char ' ' line with
  | [ d; m; c ] ->
      { dir = str_to_dir d; mag = int_of_string m; color = extract_color c }
  | _ ->
      raise
      @@ Invalid_argument
           "Unexpected format. Must be in form: \"<DIRECTION> <MAGNITUDE> \
            (#RGBHEX)\""

(* Shoelace Formula & Pick's Theorem *)
let solve plan extract =
  let rec acc (det_sum, boundary) (r', c') = function
    | [] -> (abs det_sum, boundary)
    | d :: rest ->
        let dir, mag = extract d in
        let r, c = (r', c') ++ (mag *** dir_offset dir) in
        let boundary' = boundary + mag in
        let det = (r' * c) - (c' * r) in
        acc (det + det_sum, boundary') (r, c) rest
  in
  let two_area, b = acc (0, 0) (0, 0) plan in
  (float_of_int two_area /. 2.) +. 1. +. (float_of_int b /. 2.) |> int_of_float

let part_one file =
  let lines = file_lines file |> List.map parse in
  solve lines (fun { dir; mag; _ } -> (dir, mag))

(*
   --- Part Two ---
   The Elves were right to be concerned; the planned lagoon would be much too small.

   After a few minutes, someone realizes what happened; someone swapped the color and instruction parameters when producing the dig plan. They don't have time to fix the bug; one of them asks if you can extract the correct instructions from the hexadecimal codes.

   Each hexadecimal code is six hexadecimal digits long. The first five hexadecimal digits encode the distance in meters as a five-digit hexadecimal number. The last hexadecimal digit encodes the direction to dig: 0 means R, 1 means D, 2 means L, and 3 means U.

   So, in the above example, the hexadecimal codes can be converted into the true instructions:

   #70c710 = R 461937
   #0dc571 = D 56407
   #5713f0 = R 356671
   #d2c081 = D 863240
   #59c680 = R 367720
   #411b91 = D 266681
   #8ceee2 = L 577262
   #caa173 = U 829975
   #1b58a2 = L 112010
   #caa171 = D 829975
   #7807d2 = L 491645
   #a77fa3 = U 686074
   #015232 = L 5411
   #7a21e3 = U 500254
   Digging out this loop and its interior produces a lagoon that can hold an impressive 952408144115 cubic meters of lava.

   Convert the hexadecimal color codes into the correct instructions; if the Elves follow this new dig plan, how many cubic meters of lava could the lagoon hold?
*)

let char_to_hex = function
  | c when int_of_char c <= int_of_char '9' && int_of_char c >= int_of_char '0'
    ->
      int_of_char c - int_of_char '0'
  | c when int_of_char c <= int_of_char 'z' && int_of_char c >= int_of_char 'a'
    ->
      10 + int_of_char c - int_of_char 'a'
  | _ -> raise @@ Invalid_argument "Unexpected hex char."

let hex s =
  char_list_of_string s |> List.rev
  |> List.fold_left
       (fun (i, acc) c ->
         (i + 1, acc + (int_of_float (16. ** float_of_int i) * char_to_hex c)))
       (0, 0)
  |> snd

let extract s = (String.sub s 0 5, String.sub s 5 1)

let part_two file =
  let lines = file_lines file |> List.map parse in
  solve lines (fun { color; _ } ->
      let s, c = extract color in
      (str_to_dir c, hex s))
