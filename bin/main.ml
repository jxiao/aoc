let flag_opt_prefix = "-"

type flag = Year | Day | Part | FileFn

module FM = Map.Make (struct
  type t = flag

  let compare = compare
end)

let flag_of_string = function
  | "-y" -> Year
  | "-d" -> Day
  | "-p" -> Part
  | "-f" -> FileFn
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

let part_of_int = function
  | 1 -> P1
  | 2 -> P2
  | d ->
      raise
      @@ Invalid_argument (Printf.sprintf "Part must be 1 or 2. Got: %d" d)

let string_of_part = function P1 -> "1" | P2 -> "2"
let year_to_str = function Y2023 -> "2023" | Y2024 -> "2024"

let str_to_year = function
  | "2023" -> Y2023
  | "2024" -> Y2024
  | _ -> raise @@ Invalid_argument "Unknown year string."

let opt_to_string f opt = match opt with Some v -> f v | None -> "N/A"
let command_line_args = parse_command_line_args ()
let filepath y = Printf.sprintf "files/%s/day%d.txt" @@ year_to_str y

let yr, day, part, filefn =
  let y =
    match FM.find_opt Year command_line_args with
    | None -> None
    | Some years -> Some (List.hd years |> str_to_year)
  in
  let d =
    match FM.find_opt Day command_line_args with
    | None -> None
    | Some days -> Some (List.hd days |> int_of_string)
  in
  let p =
    match FM.find_opt Part command_line_args with
    | None -> None
    | Some parts -> Some (List.hd parts |> int_of_string |> part_of_int)
  in
  let f =
    match FM.find_opt FileFn command_line_args with
    | None -> None
    | Some parts -> Some (fun _ _ -> List.hd parts)
  in
  (y, d, p, f)

let print_header () =
  Printf.printf
    "Running `main.ml` with the following optional args:\n\
     \tYear:\t%s\n\
     \tDay:\t%s\n\
     \tPart:\t%s\n\
     \tFileFn:\t%s\n\n\
     %!"
    (opt_to_string year_to_str yr)
    (opt_to_string string_of_int day)
    (opt_to_string string_of_part part)
    (opt_to_string
       (fun f ->
         f
           (match yr with None -> failwith "None" | Some y -> y)
           (match day with None -> failwith "None" | Some d -> d))
       filefn)
;;

print_header ()

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
            ( 25,
              {
                p1 = Y2023.Day25.part_one;
                p2 = (fun _ -> failwith "Merry X-Mas!");
              } );
          ] );
    ]

type result = { day : int; part : part; answer : int; time : float }

let benchmark f =
  let start = Sys.time () in
  let ans = f () in
  let finish = Sys.time () in
  (ans, finish -. start)

let exec_part mods year day part filefn =
  let path = filefn year day in
  match (IM.find_opt day mods, part) with
  | Some { p1; _ }, P1 ->
      let answer, time = benchmark (fun () -> p1 path) in
      { day; part = P1; answer; time }
  | Some { p2; _ }, P2 ->
      if day <> 25 then
        let answer, time = benchmark (fun () -> p2 path) in
        { day; part = P2; answer; time }
      else { day; part = P2; answer = 0; time = 0. }
  | None, _ ->
      raise
      @@ Invalid_argument (Printf.sprintf "Cannot find day %d in mods." day)

let len = 89

let print_result { day; part; answer; time } =
  let ans_str = string_of_int answer in
  Printf.printf "|\t%i\t|\t%s\t|\t%i%s\t\t|\t%.5fs\t|\n" day
    (string_of_part part) answer
    (if String.length ans_str < 8 then "\t" else "")
    time;
  print_endline (String.init len (fun _ -> '-'))

let print_table_header () =
  print_endline (String.init len (fun _ -> '='));
  print_endline
    "||     DAY\t|      PART\t|\tANSWER\t\t\t|         TIME         ||";
  print_endline (String.init len (fun _ -> '='))

let exec_year year =
  match YM.find_opt year modules with
  | None ->
      raise
      @@ Invalid_argument
           (Printf.sprintf "Cannot find modules for year %s" (year_to_str year))
  | Some mods -> (
      print_table_header ();
      let fn = match filefn with None -> filepath | Some f -> f in
      match (day, part) with
      | None, None ->
          IM.bindings mods
          |> List.iter (fun (d, _) ->
                 print_result @@ exec_part mods year d P1 fn;
                 print_result @@ exec_part mods year d P2 fn)
      | Some d, Some p -> print_result @@ exec_part mods year d p fn
      | Some d, None ->
          print_result @@ exec_part mods year d P1 fn;
          print_result @@ exec_part mods year d P2 fn
      | None, Some p ->
          IM.bindings mods
          |> List.iter (fun (d, _) ->
                 print_result @@ exec_part mods year d p fn))

let () =
  match yr with
  | None -> YM.iter (fun year _ -> exec_year year) modules
  | Some year -> exec_year year
