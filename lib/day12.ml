(*
   --- Day 12: Hot Springs ---
   You finally reach the hot springs! You can see steam rising from secluded areas attached to the primary, ornate building.

   As you turn to enter, the researcher stops you. "Wait - I thought you were looking for the hot springs, weren't you?" You indicate that this definitely looks like hot springs to you.

   "Oh, sorry, common mistake! This is actually the onsen! The hot springs are next door."

   You look in the direction the researcher is pointing and suddenly notice the massive metal helixes towering overhead. "This way!"

   It only takes you a few more steps to reach the main gate of the massive fenced-off area containing the springs. You go through the gate and into a small administrative building.

   "Hello! What brings you to the hot springs today? Sorry they're not very hot right now; we're having a lava shortage at the moment." You ask about the missing machine parts for Desert Island.

   "Oh, all of Gear Island is currently offline! Nothing is being manufactured at the moment, not until we get more lava to heat our forges. And our springs. The springs aren't very springy unless they're hot!"

   "Say, could you go up and see why the lava stopped flowing? The springs are too cold for normal operation, but we should be able to find one springy enough to launch you up there!"

   There's just one problem - many of the springs have fallen into disrepair, so they're not actually sure which springs would even be safe to use! Worse yet, their condition records of which springs are damaged (your puzzle input) are also damaged! You'll need to help them repair the damaged records.

   In the giant field just outside, the springs are arranged into rows. For each row, the condition records show every spring and whether it is operational (.) or damaged (#). This is the part of the condition records that is itself damaged; for some springs, it is simply unknown (?) whether the spring is operational or damaged.

   However, the engineer that produced the condition records also duplicated some of this information in a different format! After the list of springs for a given row, the size of each contiguous group of damaged springs is listed in the order those groups appear in the row. This list always accounts for every damaged spring, and each number is the entire size of its contiguous group (that is, groups are always separated by at least one operational spring: #### would always be 4, never 2,2).

   So, condition records with no unknown spring conditions might look like this:

   #.#.### 1,1,3
   .#...#....###. 1,1,3
   .#.###.#.###### 1,3,1,6
   ####.#...#... 4,1,1
   #....######..#####. 1,6,5
   .###.##....# 3,2,1
   However, the condition records are partially damaged; some of the springs' conditions are actually unknown (?). For example:

   ???.### 1,1,3
   .??..??...?##. 1,1,3
   ?#?#?#?#?#?#?#? 1,3,1,6
   ????.#...#... 4,1,1
   ????.######..#####. 1,6,5
   ?###???????? 3,2,1
   Equipped with this information, it is your job to figure out how many different arrangements of operational and broken springs fit the given criteria in each row.

   In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three broken springs (in that order) can appear in that row: the first three unknown springs must be broken, then operational, then broken (#.#), making the whole row #.#.###.

   The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different arrangements. The last ? must always be broken (to satisfy the final contiguous group of three broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be both broken springs or they would form a single contiguous group of two; if that were true, the numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are four possible arrangements of springs.

   The last line is actually consistent with ten different arrangements! Because the first number is 3, the first and second ? must both be . (if either were #, the first number would have to be 4 or higher). However, the remaining run of unknown spring conditions have many different ways they could hold groups of two and one broken springs:

   ?###???????? 3,2,1
   .###.##.#...
   .###.##..#..
   .###.##...#.
   .###.##....#
   .###..##.#..
   .###..##..#.
   .###..##...#
   .###...##.#.
   .###...##..#
   .###....##.#
   In this example, the number of possible arrangements for each row is:

   ???.### 1,1,3 - 1 arrangement
   .??..??...?##. 1,1,3 - 4 arrangements
   ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
   ????.#...#... 4,1,1 - 1 arrangement
   ????.######..#####. 1,6,5 - 4 arrangements
   ?###???????? 3,2,1 - 10 arrangements
   Adding all of the possible arrangement counts together produces a total of 21 arrangements.

   For each row, count all of the different arrangements of operational and broken springs that meet the given criteria. What is the sum of those counts?
*)

open Utils

type condition = Operational | Damaged | Unknown

let char_to_condition = function
  | '.' -> Operational
  | '#' -> Damaged
  | '?' -> Unknown
  | _ -> Invalid_argument "Cannot translate char to condition" |> raise

let cond_to_char = function
  | Operational -> '.'
  | Damaged -> '#'
  | Unknown -> '?'

let rec can_be_cond cond n l =
  if n = 0 then true
  else
    match l with
    | h :: t -> (h = Unknown || h = cond) && can_be_cond cond (n - 1) t
    | _ -> false

let damaged_seq l n =
  if List.length l < n then None
  else
    let opt, _ =
      List.fold_left
        (fun (acc, i) h ->
          match acc with
          | None -> (None, i + 1)
          | Some _ when i < n ->
              if h = Damaged || h = Unknown then (acc, i + 1) else (None, i + 1)
          | Some _ when i = n ->
              if h = Operational || h = Unknown then (acc, i + 1)
              else (None, i + 1)
          | Some l' -> (Some (h :: l'), i + 1))
        (Some [], 0) l
    in
    match opt with None -> None | Some l' -> Some (List.rev l')

module P = struct
  type t = condition list * int list

  let compare = compare
end

let save_and_return cache k v =
  Hashtbl.add cache k v;
  v

let rec calc springs groups cache =
  if Hashtbl.mem cache (springs, groups) then
    Hashtbl.find cache (springs, groups)
  else
    match (springs, groups) with
    | hs :: ts, hg :: tg -> (
        match hs with
        | Operational ->
            calc ts groups cache |> save_and_return cache (springs, groups)
        | Damaged -> (
            match damaged_seq springs hg with
            | None -> save_and_return cache (springs, groups) 0
            | Some rest ->
                calc rest tg cache |> save_and_return cache (springs, groups))
        | Unknown ->
            let curr_damaged =
              match damaged_seq springs hg with
              | None -> 0
              | Some rest -> calc rest tg cache
            in
            let curr_op = calc ts groups cache in
            save_and_return cache (springs, groups) (curr_damaged + curr_op))
    | [], _ -> if List.for_all (( = ) 0) groups then 1 else 0
    | _, [] -> if List.for_all (( <> ) Damaged) springs then 1 else 0

let repeat n sep l =
  List.init n (fun _ -> l)
  |> List.fold_left (fun acc x -> acc @ (if acc = [] then [] else sep) @ x) []

let parts repeats line =
  match String.split_on_char ' ' line with
  | [ pre; post ] ->
      let springs =
        char_list_of_string pre |> List.map char_to_condition
        |> repeat repeats [ Unknown ]
      in
      let groups =
        String.split_on_char ',' post
        |> List.map int_of_string |> repeat repeats []
      in
      (springs, groups)
  | _ ->
      Invalid_argument "Could not find separator between springs and groups"
      |> raise

let arrangements repeats line =
  let springs, groups = parts repeats line in
  Hashtbl.create (List.length springs * List.length groups)
  |> calc springs groups

let part_one file =
  let lines = file_lines file in
  let res = List.map (arrangements 1) lines in
  sum res

(*
   --- Part Two ---
   As you look out at the field of springs, you feel like there are way more springs than the condition records list. When you examine the records, you discover that they were actually folded up this whole time!

   To unfold the records, on each row, replace the list of spring conditions with five copies of itself (separated by ?) and replace the list of contiguous groups of damaged springs with five copies of itself (separated by ,).

   So, this row:

   .# 1
   Would become:

   .#?.#?.#?.#?.# 1,1,1,1,1
   The first line of the above example would become:

   ???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
   In the above example, after unfolding, the number of possible arrangements for some rows is now much larger:

   ???.### 1,1,3 - 1 arrangement
   .??..??...?##. 1,1,3 - 16384 arrangements
   ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
   ????.#...#... 4,1,1 - 16 arrangements
   ????.######..#####. 1,6,5 - 2500 arrangements
   ?###???????? 3,2,1 - 506250 arrangements
   After unfolding, adding all of the possible arrangement counts together produces 525152.

   Unfold your condition records; what is the new sum of possible arrangement counts?
*)

let part_two file =
  let lines = file_lines file in
  let res = List.map (arrangements 5) lines in
  sum res
