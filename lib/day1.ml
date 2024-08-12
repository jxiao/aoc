(* 
--- Day 1: Trebuchet?! ---
Something is wrong with global snow production, and you've been selected to take a look. The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").

As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of the calibration values?
*)

let char_list_of_string s = 
  s |> String.to_seq |> List.of_seq

let file_lines name = 
  let channel = open_in name in
  let read_line () = try Some (input_line channel) with End_of_file -> None in
  let rec read_lines acc =
    match read_line () with
    | None -> List.rev acc
    | Some line -> line :: acc |> read_lines
  in
  read_lines []

let digit_opt c =
  match c with
  | '0'..'9' -> Some (int_of_char c - int_of_char '0')
  | _ -> None 

let extract_num s = 
  let rec extract first second l = 
    match first, second, l with
    | None, _, [] -> Invalid_argument "Input string does not have 2 digits." |> raise
    | None, Some _, _ -> Invalid_argument "Invalid state: first=None, second=Some" |> raise
    | Some d1, Some d2, [] -> d1 * 10 + d2
    | Some d1, None, [] -> d1 * 11
    | None, None, hd::tl -> extract (digit_opt hd) None tl
    | Some _, _, hd::tl -> 
      begin match digit_opt hd with
      | Some d3 -> extract first (Some d3) tl
      | None -> extract first second tl
      end
  in 
  extract None None (char_list_of_string s)

let calibration_vals = List.map extract_num
let sum = List.fold_left (+) 0

let part_one_result ?(debug=false) file = 
  let lines = file_lines file in
  let calibrations = calibration_vals lines in
  let answer = sum calibrations in
  if debug then List.combine lines calibrations
    |> Utils.print_list (fun (s,i) -> Printf.sprintf "(%s,%i)\n%!" s i);
   answer

(* 
--- Part Two ---
Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

What is the sum of all of the calibration values?
*)

(* let m = [("zero",0);("one",1)] *)

(* let starts_with_opt =  *)