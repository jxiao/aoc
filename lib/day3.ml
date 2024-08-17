open Utils
(*
   --- Day 3: Gear Ratios ---
   You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you. You go inside.

   It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

   "Aaah!"

   You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

   The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

   The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

   Here is an example engine schematic:

   467..114..
   ...*......
   ..35..633.
   ......#...
   617*......
   .....+.58.
   ..592.....
   ......755.
   ...$.*....
   .664.598..
   In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

   Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?
*)

let is_digit = function '0' .. '9' -> true | _ -> false
let is_symbol = function '0' .. '9' | '.' -> false | _ -> true

module Tup = struct
  type t = int * int

  let compare = compare
end

module TSet = Set.Make (Tup)

let symbol_positions lines =
  let rec parse_row r c l set =
    match l with
    | [] -> set
    | h :: t ->
        parse_row r (c + 1) t (if is_symbol h then TSet.add (r, c) set else set)
  in
  let rec parse_rows r l set =
    match l with
    | [] -> set
    | h :: t ->
        parse_row r 0 (char_list_of_string h) set |> parse_rows (r + 1) t
  in
  parse_rows 0 lines TSet.empty

let adjs =
  List.fold_left
    (fun acc r -> acc @ List.map (fun c -> (r, c)) [ -1; 0; 1 ])
    [] [ -1; 0; 1 ]

let adj_to_sym positions r c =
  let rec acc = function
    | [] -> false
    | (dr, dc) :: t -> TSet.mem (r + dr, c + dc) positions || acc t
  in
  acc adjs

let rec traverse line positions acc should_add sum (r, c) =
  match line with
  | [] -> sum + if should_add then acc else 0
  | h :: t -> (
      match digit_opt h with
      | Some d ->
          let is_adj_sym = adj_to_sym positions r c in
          traverse t positions
            ((acc * 10) + d)
            (is_adj_sym || should_add) sum
            (r, c + 1)
      | None ->
          traverse t positions 0 false
            (sum + if should_add then acc else 0)
            (r, c + 1))

let part_one file =
  let lines = file_lines file in
  let positions = symbol_positions lines in
  let row_scores =
    List.mapi
      (fun i line ->
        traverse (char_list_of_string line) positions 0 false 0 (i, 0))
      lines
  in
  sum row_scores

(*
   --- Part Two ---
   The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.

   You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.

   Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

   The missing part wasn't the only issue - one of the gears in the engine is wrong. A gear is any * symbol that is adjacent to exactly two part numbers. Its gear ratio is the result of multiplying those two numbers together.

   This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.

   Consider the same engine schematic again:

   467..114..
   ...*......
   ..35..633.
   ......#...
   617*......
   .....+.58.
   ..592.....
   ......755.
   ...$.*....
   .664.598..
   In this schematic, there are two gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is not a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces 467835.

   What is the sum of all of the gear ratios in your engine schematic?
*)

let is_star = ( = ) '*'

module TMap = Map.Make (Tup)

(** [num_positions lines] is the mapping from [(r,c)] coordinate to 
    [(id, value)], where [id] is a unique id per number and [value] represents 
    the number itself (which can span multiple coordinate positions. *)
let num_positions lines =
  let rec parse_row line (r, c) mapping positions acc next_id =
    match line with
    | [] ->
        ( List.fold_left
            (fun res tup -> TMap.add tup (next_id, acc) res)
            mapping positions,
          next_id + 1 )
    | h :: t -> (
        match digit_opt h with
        | None ->
            parse_row t
              (r, c + 1)
              (List.fold_left
                 (fun res tup -> TMap.add tup (next_id, acc) res)
                 mapping positions)
              [] 0 (next_id + 1)
        | Some d ->
            parse_row t
              (r, c + 1)
              mapping ((r, c) :: positions)
              ((acc * 10) + d)
              next_id)
  in
  let rec parse_rows lines mapping r next_id =
    match lines with
    | [] -> mapping
    | h :: t ->
        let mapping', next_id' =
          parse_row (char_list_of_string h) (r, 0) mapping [] 0 next_id
        in
        parse_rows t mapping' (r + 1) next_id'
  in
  parse_rows lines TMap.empty 0 0

let get_surrounding_nums (r, c) mappings =
  let rec build l set =
    match l with
    | [] -> set
    | (dr, dc) :: t ->
        build t
          (match TMap.find_opt (r + dr, c + dc) mappings with
          | None -> set
          | Some v -> TSet.add v set)
  in
  build adjs TSet.empty

let rec gear_ratio_for_row row acc mappings (r, c) =
  match row with
  | [] -> acc
  | h :: t ->
      let gear_ratio =
        if is_star h then
          let adj_set = get_surrounding_nums (r, c) mappings in
          if TSet.cardinal adj_set = 2 then
            TSet.fold (fun (_, v) res -> v * res) adj_set 1
          else 0
        else 0
      in
      gear_ratio_for_row t (acc + gear_ratio) mappings (r, c + 1)

let part_two file =
  let lines = file_lines file in
  let mappings = num_positions lines in
  let row_scores =
    List.mapi
      (fun i line ->
        gear_ratio_for_row (char_list_of_string line) 0 mappings (i, 0))
      lines
  in
  sum row_scores
