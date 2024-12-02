(*
   --- Day 14: Parabolic Reflector Dish ---
   You reach the place where all of the mirrors were pointing: a massive parabolic reflector dish attached to the side of another large mountain.

   The dish is made up of many small mirrors, but while the mirrors themselves are roughly in the shape of a parabolic reflector dish, each individual mirror seems to be pointing in slightly the wrong direction. If the dish is meant to focus light, all it's doing right now is sending it in a vague direction.

   This system must be what provides the energy for the lava! If you focus the reflector dish, maybe you can go where it's pointing and use the light to fix the lava production.

   Upon closer inspection, the individual mirrors each appear to be connected via an elaborate system of ropes and pulleys to a large metal platform below the dish. The platform is covered in large rocks of various shapes. Depending on their position, the weight of the rocks deforms the platform, and the shape of the platform controls which ropes move and ultimately the focus of the dish.

   In short: if you move the rocks, you can focus the dish. The platform even has a control panel on the side that lets you tilt it in one of four directions! The rounded rocks (O) will roll when the platform is tilted, while the cube-shaped rocks (#) will stay in place. You note the positions of all of the empty spaces (.) and rocks (your puzzle input). For example:

   O....#....
   O.OO#....#
   .....##...
   OO.#O....O
   .O.....O#.
   O.#..O.#.#
   ..O..#O..O
   .......O..
   #....###..
   #OO..#....
   Start by tilting the lever so all of the rocks will slide north as far as they will go:

   OOOO.#.O..
   OO..#....#
   OO..O##..O
   O..#.OO...
   ........#.
   ..#....#.#
   ..O..#.O.O
   ..O.......
   #....###..
   #....#....
   You notice that the support beams along the north side of the platform are damaged; to ensure the platform doesn't collapse, you should calculate the total load on the north support beams.

   The amount of load caused by a single rounded rock (O) is equal to the number of rows from the rock to the south edge of the platform, including the row the rock is on. (Cube-shaped rocks (#) don't contribute to load.) So, the amount of load caused by each rock in each row is as follows:

   OOOO.#.O.. 10
   OO..#....#  9
   OO..O##..O  8
   O..#.OO...  7
   ........#.  6
   ..#....#.#  5
   ..O..#.O.O  4
   ..O.......  3
   #....###..  2
   #....#....  1
   The total load is the sum of the load caused by all of the rounded rocks. In this example, the total load is 136.

   Tilt the platform so that the rounded rocks all roll north. Afterward, what is the total load on the north support beams?
*)

open Utils

(* NOTE: copied from Day 13 *)
let transpose mat =
  let mapping = Hashtbl.create (List.length mat * List.length mat) in
  List.iter
    (fun row ->
      List.iteri
        (fun c v ->
          match Hashtbl.find_opt mapping c with
          | None -> Hashtbl.add mapping c [ v ]
          | Some l -> Hashtbl.replace mapping c (l @ [ v ]))
        row)
    mat;
  Hashtbl.to_seq mapping |> List.of_seq |> List.sort compare |> List.map snd

let shift_load row =
  let n = List.length row in
  let _, _, score =
    List.fold_left
      (fun (next, i, score) c ->
        match c with
        | '.' -> (next, i - 1, score)
        | '#' -> (i - 1, i - 1, score)
        | 'O' -> (next - 1, i - 1, score + next)
        | _ -> Invalid_argument "Illegal character found." |> raise)
      (n, n, 0) row
  in
  score

let part_one file =
  let lines = file_lines file in
  let puzzle = List.map char_list_of_string lines |> transpose in
  List.map shift_load puzzle |> sum

(*
   --- Part Two ---
   The parabolic reflector dish deforms, but not in a way that focuses the beam. To do that, you'll need to move the rocks to the edges of the platform. Fortunately, a button on the side of the control panel labeled "spin cycle" attempts to do just that!

   Each cycle tilts the platform four times so that the rounded rocks roll north, then west, then south, then east. After each tilt, the rounded rocks roll as far as they can before the platform tilts in the next direction. After one cycle, the platform will have finished rolling the rounded rocks in those four directions in that order.

   Here's what happens in the example above after each of the first few cycles:

   After 1 cycle:
   .....#....
   ....#...O#
   ...OO##...
   .OO#......
   .....OOO#.
   .O#...O#.#
   ....O#....
   ......OOOO
   #...O###..
   #..OO#....

   After 2 cycles:
   .....#....
   ....#...O#
   .....##...
   ..O#......
   .....OOO#.
   .O#...O#.#
   ....O#...O
   .......OOO
   #..OO###..
   #.OOO#...O

   After 3 cycles:
   .....#....
   ....#...O#
   .....##...
   ..O#......
   .....OOO#.
   .O#...O#.#
   ....O#...O
   .......OOO
   #...O###.O
   #.OOO#...O
   This process should work if you leave it running long enough, but you're still worried about the north support beams. To make sure they'll survive for a while, you need to calculate the total load on the north support beams after 1000000000 cycles.

   In the above example, after 1000000000 cycles, the total load on the north support beams is 64.

   Run the spin cycle for 1000000000 cycles. Afterward, what is the total load on the north support beams?
*)

let shift =
  List.map (fun row ->
      let parts =
        List.to_seq row |> String.of_seq |> String.split_on_char '#'
      in
      let shifted =
        List.map
          (fun s ->
            let num_Os = (String.split_on_char 'O' s |> List.length) - 1 in
            List.init (String.length s) (fun i ->
                if i < num_Os then 'O' else '.')
            |> List.to_seq |> String.of_seq)
          parts
      in
      List.mapi
        (fun i s -> (if i <> 0 then "#" ^ s else s) |> char_list_of_string)
        shifted
      |> List.flatten)

let rev_mat = List.map List.rev
let north mat = transpose mat |> shift |> transpose
let west = shift
let south mat = transpose mat |> rev_mat |> shift |> rev_mat |> transpose
let east mat = rev_mat mat |> shift |> rev_mat
let cycle mat = north mat |> west |> south |> east
let rounds = 1000000000

let load row =
  let n = List.length row in
  List.fold_left
    (fun (i, score) c ->
      match c with
      | '.' -> (i - 1, score)
      | '#' -> (i - 1, score)
      | 'O' -> (i - 1, score + i)
      | _ -> Invalid_argument "Illegal character found." |> raise)
    (n, 0) row
  |> snd

let part_two file =
  let lines = file_lines file in
  let seen = Hashtbl.create 100_000 in
  let rec run i mat =
    if i = rounds || Hashtbl.mem seen mat then (i, mat)
    else (
      Hashtbl.add seen mat i;
      run (i + 1) (cycle mat))
  in
  let iters, mat = run 0 (List.map char_list_of_string lines) in
  let final_mat =
    if iters = rounds then mat
    else
      let first_mat_idx = Hashtbl.find seen mat in
      let final_mat_idx =
        first_mat_idx + ((rounds - first_mat_idx) mod (iters - first_mat_idx))
      in
      Hashtbl.to_seq seen
      |> Seq.filter_map (fun (m, i) ->
             if i = final_mat_idx then Some m else None)
      |> List.of_seq |> List.hd
  in
  final_mat |> transpose |> List.map load |> sum
