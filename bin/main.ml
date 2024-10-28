let flag_opt_prefix = "-"

type flag = Year | Day | Part

module FM = Map.Make (struct
  type t = flag

  let compare = compare
end)

let flag_of_string = function
  | "-y" -> Year
  | "-d" -> Day
  | "-p" -> Part
  | _ -> raise @@ Invalid_argument "Cannot parse string to flag"

let parse_command_line_args () =
  let rec parse curr_flag acc = function
    | [] -> acc
    | h :: t ->
        if String.starts_with ~prefix:flag_opt_prefix h then
          let f = flag_of_string h in
          parse f (FM.add f [] acc) t
        else
          parse curr_flag
            (FM.add curr_flag (FM.find curr_flag acc @ [ h ]) acc)
            t
  in
  match Sys.argv |> Array.to_list with
  | _ :: t -> parse Year FM.empty t
  | _ -> FM.empty

(* dune exec bin/main.ml -y 2023 -d 1 -p 1 *)
type year = Y2023 | Y2024

module YM = Map.Make (struct
  type t = year

  let compare = compare
end)

module IM = Map.Make (Int)

type problem = { p1 : string -> int; p2 : string -> int }
type part = P1 | P2

let year_to_str = function Y2023 -> "2023" | Y2024 -> "2024"

let str_to_year = function
  | "2023" -> Y2023
  | "2024" -> Y2024
  | _ -> raise @@ Invalid_argument "Unknown year string."

let int_opt_to_string opt =
  match opt with Some v -> string_of_int v | None -> "N/A"

let print_header y d p =
  Printf.printf
    "Running `main.ml` with the following optional args:\n\
     \tYear:\t%s\n\
     \tDay:\t%s\n\
     \tPart:\t%s\n\n\
     %!"
    (int_opt_to_string y) (int_opt_to_string d) (int_opt_to_string p)

(* Note: first arg is always the executable file path. Ignored. *)
let yr, day, part =
  match Sys.argv |> Array.to_list with
  | _ :: y :: d :: p :: _ ->
      (str_to_year y, int_of_string_opt d, int_of_string_opt p)
  | [ _; y ] -> (str_to_year y, None, None)
  | [ _; y; d ] -> (str_to_year y, int_of_string_opt d, None)
  | _ -> (Y2023, None, None)

let command_line_args = parse_command_line_args ();;

print_header day part

let print_result d p ans = Printf.printf "Day %i, Part %i = %i\n%!" d p ans
let filepath y = Printf.sprintf "files/%s/day%d.txt" @@ year_to_str y

let modules =
  YM.of_list
    [
      ( Y2023,
        IM.of_list
          [
            (1, { p1 = Y2023.Day1.part_one; p2 = Y2023.Day1.part_two });
            (2, { p1 = Y2023.Day2.part_one; p2 = Y2023.Day2.part_two });
            (3, { p1 = Y2023.Day3.part_one; p2 = Y2023.Day3.part_two });
            (4, { p1 = Y2023.Day4.part_one; p2 = Y2023.Day4.part_two });
            (5, { p1 = Y2023.Day5.part_one; p2 = Y2023.Day5.part_two });
            (6, { p1 = Y2023.Day6.part_one; p2 = Y2023.Day6.part_two });
            (7, { p1 = Y2023.Day7.part_one; p2 = Y2023.Day7.part_two });
            (8, { p1 = Y2023.Day8.part_one; p2 = Y2023.Day8.part_two });
            (9, { p1 = Y2023.Day9.part_one; p2 = Y2023.Day9.part_two });
            (10, { p1 = Y2023.Day10.part_one; p2 = Y2023.Day10.part_two });
            (11, { p1 = Y2023.Day11.part_one; p2 = Y2023.Day11.part_two });
            (12, { p1 = Y2023.Day12.part_one; p2 = Y2023.Day12.part_two });
            (13, { p1 = Y2023.Day13.part_one; p2 = Y2023.Day13.part_two });
            (14, { p1 = Y2023.Day14.part_one; p2 = Y2023.Day14.part_two });
            (15, { p1 = Y2023.Day15.part_one; p2 = Y2023.Day15.part_two });
            (16, { p1 = Y2023.Day16.part_one; p2 = Y2023.Day16.part_two });
            (17, { p1 = Y2023.Day17.part_one; p2 = Y2023.Day17.part_two });
            (18, { p1 = Y2023.Day18.part_one; p2 = Y2023.Day18.part_two });
            (19, { p1 = Y2023.Day19.part_one; p2 = Y2023.Day19.part_two });
            (20, { p1 = Y2023.Day20.part_one; p2 = Y2023.Day20.part_two });
            (21, { p1 = Y2023.Day21.part_one; p2 = Y2023.Day21.part_two });
            (22, { p1 = Y2023.Day22.part_one; p2 = Y2023.Day22.part_two });
            (23, { p1 = Y2023.Day23.part_one; p2 = Y2023.Day23.part_two });
            (24, { p1 = Y2023.Day24.part_one; p2 = Y2023.Day24.part_two });
            (25, { p1 = Y2023.Day25.part_one; p2 = failwith "Merry X-Mas!" });
          ] );
    ]

let exec_day y d parts =
  let path = filepath y d in
  match part with
  | Some part ->
      if List.length parts >= part then
        List.nth parts (part - 1) path |> print_result d part
  | None -> List.iteri (fun i f -> f path |> print_result d (i + 1)) parts
;;

match YM.find_opt yr modules with
| None ->
    raise
    @@ Invalid_argument
         (Printf.sprintf "Cannot find modules for year %s" (year_to_str yr))
| Some mods -> ( match day with None -> exec_day yr d)

(* List.iter
   (fun (d, fs) ->
     match day with
     | Some day -> if day = d then exec_day yr d fs
     | None -> exec_day yr d fs)
   modules *)
