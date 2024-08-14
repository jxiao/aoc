open Utils
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

module CL = struct
  type t = char list

  let compare = compare
end

module CLMap = Map.Make (CL)

let char_list_of_string s = s |> String.to_seq |> List.of_seq

let rec starts_with sub l =
  match (sub, l) with
  | [], _ -> true
  | _, [] -> false
  | hd1 :: tl1, hd2 :: tl2 -> hd1 = hd2 && starts_with tl1 tl2

let starts_with_num l mapping =
  let rec acc remaining =
    match remaining with
    | [] -> None
    | (chars, v) :: t -> if starts_with chars l then Some v else acc t
  in
  acc (CLMap.bindings mapping)

let extract mapping s =
  let chars = char_list_of_string s in
  let rec acc first second l =
    match (first, second, l) with
    | _, _, [] -> (first, second)
    | None, None, _ :: tl ->
        let prefix = starts_with_num l mapping in
        acc prefix prefix tl
    | Some _, Some _, _ :: tl ->
        let prefix = starts_with_num l mapping in
        if prefix = None then acc first second tl else acc first prefix tl
    | _, _, _ ->
        Invalid_argument
          "Cannot have non-empty list while first and last differ in Optional \
           values" |> raise
  in
  acc None None chars

let score (d1, d2) =
  match (d1, d2) with
  | Some d1, Some d2 -> (d1 * 10) + d2
  | None, Some d ->
      Invalid_argument
        (Printf.sprintf "Both values must be Some: (None, Some %d)" d)
      |> raise
  | Some d, None ->
      Invalid_argument
        (Printf.sprintf "Both values must be Some: (Some %d, None)" d)
      |> raise
  | None, None -> Invalid_argument "No digits found" |> raise

let get_part_result ?(debug = false) mapping file =
  let lines = file_lines file in
  let calibrations = List.map (extract mapping) lines in
  let answer = List.fold_left (fun acc p -> acc + score p) 0 calibrations in
  if debug then
    List.combine lines (List.map score calibrations)
    |> Utils.print_list (fun (s, i) -> Printf.sprintf "(%s,%i)" s i);
  answer

let digit_mappings =
  CLMap.empty |> CLMap.add [ '1' ] 1 |> CLMap.add [ '2' ] 2
  |> CLMap.add [ '3' ] 3 |> CLMap.add [ '4' ] 4 |> CLMap.add [ '5' ] 5
  |> CLMap.add [ '6' ] 6 |> CLMap.add [ '7' ] 7 |> CLMap.add [ '8' ] 8
  |> CLMap.add [ '9' ] 9

let part_one_result ?(debug = false) file =
  get_part_result ~debug digit_mappings file

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

let digit_word_mappings =
  digit_mappings
  |> CLMap.add (char_list_of_string "one") 1
  |> CLMap.add (char_list_of_string "two") 2
  |> CLMap.add (char_list_of_string "three") 3
  |> CLMap.add (char_list_of_string "four") 4
  |> CLMap.add (char_list_of_string "five") 5
  |> CLMap.add (char_list_of_string "six") 6
  |> CLMap.add (char_list_of_string "seven") 7
  |> CLMap.add (char_list_of_string "eight") 8
  |> CLMap.add (char_list_of_string "nine") 9

let part_two_result ?(debug = false) file =
  get_part_result ~debug digit_word_mappings file
