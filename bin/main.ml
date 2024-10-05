let int_opt_to_string opt =
  match opt with Some v -> string_of_int v | None -> "N/A"

let print_header d p =
  Printf.printf
    "Running `main.ml` with the following optional args:\n\
     \tDay:\t%s\n\
     \tPart:\t%s\n\n\
     %!"
    (int_opt_to_string d) (int_opt_to_string p)

(* Note: first arg is always the executable file path. Ignored. *)
let day, part =
  match Sys.argv |> Array.to_list with
  | _ :: d :: p :: _ -> (int_of_string_opt d, int_of_string_opt p)
  | [ _; d ] -> (int_of_string_opt d, None)
  | _ -> (None, None)
;;

print_header day part

let print_result d p ans = Printf.printf "Day %i, Part %i = %i\n%!" d p ans
let filepath = Printf.sprintf "files/day%d.txt"

let modules =
  [
    (1, [ Aoc.Day1.part_one; Aoc.Day1.part_two ]);
    (2, [ Aoc.Day2.part_one; Aoc.Day2.part_two ]);
    (3, [ Aoc.Day3.part_one; Aoc.Day3.part_two ]);
    (4, [ Aoc.Day4.part_one; Aoc.Day4.part_two ]);
    (5, [ Aoc.Day5.part_one; Aoc.Day5.part_two ]);
    (6, [ Aoc.Day6.part_one; Aoc.Day6.part_two ]);
    (7, [ Aoc.Day7.part_one; Aoc.Day7.part_two ]);
    (8, [ Aoc.Day8.part_one; Aoc.Day8.part_two ]);
    (9, [ Aoc.Day9.part_one; Aoc.Day9.part_two ]);
    (10, [ Aoc.Day10.part_one; Aoc.Day10.part_two ]);
    (11, [ Aoc.Day11.part_one; Aoc.Day11.part_two ]);
    (12, [ Aoc.Day12.part_one; Aoc.Day12.part_two ]);
    (13, [ Aoc.Day13.part_one; Aoc.Day13.part_two ]);
    (14, [ Aoc.Day14.part_one; Aoc.Day14.part_two ]);
    (15, [ Aoc.Day15.part_one; Aoc.Day15.part_two ]);
    (16, [ Aoc.Day16.part_one; Aoc.Day16.part_two ]);
    (17, [ Aoc.Day17.part_one; Aoc.Day17.part_two ]);
  ]

let exec_day d parts =
  let path = filepath d in
  match part with
  | Some part ->
      if List.length parts >= part then
        List.nth parts (part - 1) path |> print_result d part
  | None -> List.iteri (fun i f -> f path |> print_result d (i + 1)) parts
;;

List.iter
  (fun (d, fs) ->
    match day with
    | Some day -> if day = d then exec_day d fs
    | None -> exec_day d fs)
  modules
