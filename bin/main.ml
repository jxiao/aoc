let print_result day part ans =
  Printf.printf "Day %i, Part %i = %i\n%!" day part ans

let day1 = "files/day1.txt";;

Aoc.Day1.part_one_result day1 |> print_result 1 1;;
Aoc.Day1.part_two_result day1 |> print_result 1 2

let day2 = "files/day2.txt";;

Aoc.Day2.part_one_result day2 |> print_result 2 1;;
Aoc.Day2.part_two_result day2 |> print_result 2 2

let day3 = "files/day3.txt";;

Aoc.Day3.part_one day3 |> print_result 3 1;;
Aoc.Day3.part_two day3 |> print_result 3 2

let day4 = "files/day4.txt";;

Aoc.Day4.part_one day4 |> print_result 4 1;;
Aoc.Day4.part_two day4 |> print_result 4 2
