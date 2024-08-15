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

let digit_opt c =
  match c with
  | '0' .. '9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None

let is_symbol = function '0' .. '9' | '.' -> false | _ -> true

module Tup = struct
  type t = int * int

  let compare = compare
end

module TSet = Set.Make (Tup)

let symbol_positions lines =
  let rec parse_line r c l set =
    match l with
    | [] -> set
    | h :: t ->
        parse_line r (c + 1) t
          (if is_symbol h then TSet.add (r, c) set else set)
  in
  let rec build r l set =
    match l with
    | [] -> set
    | h :: t -> parse_line r 0 (char_list_of_string h) set |> build (r + 1) t
  in
  build 0 lines TSet.empty

let adj_sym positions r c =
  let adjs =
    List.fold_left
      (fun acc r -> acc @ List.map (fun c -> (r, c)) [ -1; 0; 1 ])
      [] [ -1; 0; 1 ]
  in
  let rec acc = function
    | [] -> false
    | (dr, dc) :: t -> TSet.mem (r + dr, c + dc) positions || acc t
  in
  acc adjs

let rec traverse line positions acc should_add sum r c =
  match line with
  | [] -> sum + if should_add then acc else 0
  | h :: t -> (
      match digit_opt h with
      | Some d ->
          let is_adj_sym = adj_sym positions r c in
          if is_adj_sym then
            traverse t positions ((acc * 10) + d) true sum r (c + 1)
          else traverse t positions 0 false (acc + sum) r (c + 1)
      | None ->
          traverse t positions 0 false
            (acc + if should_add then acc else 0)
            r (c + 1))

let score file =
  let lines = file_lines file in
  let positions = symbol_positions lines in
  let row_scores =
    List.mapi
      (fun i line ->
        traverse (char_list_of_string line) positions 0 false 0 i 0)
      lines
  in
  print_list (fun v -> Printf.sprintf "%d -" v) row_scores;
  let score = List.fold_left ( + ) 0 row_scores in
  Printf.printf "Final score: %i\n%!" score;
  score

(* if digit, scan area around. if symbol present, mark should_add to be true. rec call (acc * 10) true sum r c+1.
   if not digit, add should_add and mark as false. reset to 0, continue *)
