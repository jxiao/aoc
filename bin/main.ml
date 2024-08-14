let print_result day part ans =
  Printf.printf "Day %i, Part %i = %i\n%!" day part ans

let day1 = "files/day1.txt";;

Aoc.Day1.part_one_result day1 |> print_result 1 1;;
Aoc.Day1.part_two_result day1 |> print_result 1 2

let day2 = "files/day2.txt";;

Aoc.Day2.part_one_result day2 |> print_result 2 1;;
Aoc.Day2.part_two_result day2 |> print_result 2 2
