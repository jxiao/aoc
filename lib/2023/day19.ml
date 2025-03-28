(*
   --- Day 19: Aplenty ---
   The Elves of Gear Island are thankful for your help and send you on your way. They even have a hang glider that someone stole from Desert Island; since you're already going that direction, it would help them a lot if you would use it to get down there and return it to them.

   As you reach the bottom of the relentless avalanche of machine parts, you discover that they're already forming a formidable heap. Don't worry, though - a group of Elves is already here organizing the parts, and they have a system.

   To start, each part is rated in each of four categories:

   x: Extremely cool looking
   m: Musical (it makes a noise when you hit it)
   a: Aerodynamic
   s: Shiny
   Then, each part is sent through a series of workflows that will ultimately accept or reject the part. Each workflow has a name and contains a list of rules; each rule specifies a condition and where to send the part if the condition is true. The first rule that matches the part being considered is applied immediately, and the part moves on to the destination described by the rule. (The last rule in each workflow has no condition and always applies if reached.)

   Consider the workflow ex{x>10:one,m<20:two,a>30:R,A}. This workflow is named ex and contains four rules. If workflow ex were considering a specific part, it would perform the following steps in order:

   Rule "x>10:one": If the part's x is more than 10, send the part to the workflow named one.
   Rule "m<20:two": Otherwise, if the part's m is less than 20, send the part to the workflow named two.
   Rule "a>30:R": Otherwise, if the part's a is more than 30, the part is immediately rejected (R).
   Rule "A": Otherwise, because no other rules matched the part, the part is immediately accepted (A).
   If a part is sent to another workflow, it immediately switches to the start of that workflow instead and never returns. If a part is accepted (sent to A) or rejected (sent to R), the part immediately stops any further processing.

   The system works, but it's not keeping up with the torrent of weird metal shapes. The Elves ask if you can help sort a few parts and give you the list of workflows and some part ratings (your puzzle input). For example:

   px{a<2006:qkq,m>2090:A,rfg}
   pv{a>1716:R,A}
   lnx{m>1548:A,A}
   rfg{s<537:gd,x>2440:R,A}
   qs{s>3448:A,lnx}
   qkq{x<1416:A,crn}
   crn{x>2662:A,R}
   in{s<1351:px,qqz}
   qqz{s>2770:qs,m<1801:hdj,R}
   gd{a>3333:R,R}
   hdj{m>838:A,pv}

   {x=787,m=2655,a=1222,s=2876}
   {x=1679,m=44,a=2067,s=496}
   {x=2036,m=264,a=79,s=2244}
   {x=2461,m=1339,a=466,s=291}
   {x=2127,m=1623,a=2188,s=1013}
   The workflows are listed first, followed by a blank line, then the ratings of the parts the Elves would like you to sort. All parts begin in the workflow named in. In this example, the five listed parts go through the following workflows:

   {x=787,m=2655,a=1222,s=2876}: in -> qqz -> qs -> lnx -> A
   {x=1679,m=44,a=2067,s=496}: in -> px -> rfg -> gd -> R
   {x=2036,m=264,a=79,s=2244}: in -> qqz -> hdj -> pv -> A
   {x=2461,m=1339,a=466,s=291}: in -> px -> qkq -> crn -> R
   {x=2127,m=1623,a=2188,s=1013}: in -> px -> rfg -> A
   Ultimately, three parts are accepted. Adding up the x, m, a, and s rating for each of the accepted parts gives 7540 for the part with x=787, 4623 for the part with x=2036, and 6951 for the part with x=2127. Adding all of the ratings for all of the accepted parts gives the sum total of 19114.

   Sort through all of the parts you've been given; what do you get if you add together all of the rating numbers for all of the parts that ultimately get accepted?
*)

open Utils

type category = X | M | A | S
type comparator = LT | GT
type result = Reject | Accept | Workflow of string
type condition = NA | Cond of category * comparator * int
type rule = condition * result

module C = struct
  type t = category

  let compare = compare
end

module CMap = Map.Make (C)
module SMap = Map.Make (String)

let char_to_category = function
  | 'x' -> X
  | 'm' -> M
  | 'a' -> A
  | 's' -> S
  | c ->
      raise
      @@ Invalid_argument
           (Printf.sprintf "Unknown char. Cannot map to category: %s"
              (string_of_char c))

let char_to_comp = function
  | '<' -> LT
  | '>' -> GT
  | _ -> raise @@ Invalid_argument "Unknown char. Cannot map to comparator."

let string_to_result = function
  | "A" -> Accept
  | "R" -> Reject
  | s -> Workflow s

let string_to_condition s =
  match char_list_of_string s with
  | [] -> NA
  | cat :: comp :: rest ->
      Cond
        ( char_to_category cat,
          char_to_comp comp,
          int_of_string (List.to_seq rest |> String.of_seq) )
  | _ -> failwith ""

let parse_rule m s =
  let brace_idx = String.index s '{' in
  let name = String.sub s 0 brace_idx in
  let rules = String.sub s (brace_idx + 1) (String.length s - brace_idx - 2) in
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
        aux
          ((match String.split_on_char ':' h with
           | [ res ] -> (NA, string_to_result res)
           | [ cond; res ] -> (string_to_condition cond, string_to_result res)
           | _ -> raise @@ Invalid_argument "Unknown condition/rule schema")
          :: acc)
          t
  in
  SMap.add name (aux [] (String.split_on_char ',' rules)) m

let build_rules rules =
  let rec aux m = function [] -> m | h :: t -> aux (parse_rule m h) t in
  aux SMap.empty rules

let parse_part s =
  let no_braces = String.sub s 1 (String.length s - 2) in
  String.split_on_char ',' no_braces
  |> List.map (String.split_on_char '=')
  |> List.map (fun l ->
         match l with
         | [ v; num ] ->
             ( char_list_of_string v |> List.hd |> char_to_category,
               int_of_string num )
         | _ -> raise @@ Invalid_argument "Unknown schema for workflow.")
  |> List.fold_left (fun acc (cat, v) -> CMap.add cat v acc) CMap.empty

let split lines =
  let _, rules, parts =
    List.fold_left
      (fun (is_workflow, rules, parts) s ->
        if String.length s = 0 then (false, rules, parts)
        else if is_workflow then (true, s :: rules, parts)
        else (false, rules, s :: parts))
      (true, [], []) lines
  in
  (rules, parts)

let eval_condition parts (cat, comp, v) =
  match comp with
  | LT -> CMap.find cat parts < v
  | GT -> CMap.find cat parts > v

let solve_part rules part =
  let rec aux = function
    | [ (NA, Workflow s) ] -> aux (SMap.find s rules)
    | [ (NA, res) ] -> res
    | (Cond (cat, comp, v), res) :: t -> (
        match (eval_condition part (cat, comp, v), res) with
        | true, Accept | true, Reject -> res
        | true, Workflow s -> aux (SMap.find s rules)
        | false, _ -> aux t)
    | _ -> raise @@ Invalid_argument "Indeterminate."
  in
  aux (SMap.find "in" rules)

let part_one file =
  let lines = file_lines file in
  let r, p = split lines in
  let rules, parts = (build_rules r, List.map parse_part p) in
  List.filter_map
    (fun part ->
      match solve_part rules part with
      | Accept -> Some (CMap.bindings part |> List.map snd |> sum)
      | _ -> None)
    parts
  |> sum

(*
   --- Part Two ---
   Even with your help, the sorting process still isn't fast enough.

   One of the Elves comes up with a new plan: rather than sort parts individually through all of these workflows, maybe you can figure out in advance which combinations of ratings will be accepted or rejected.

   Each of the four ratings (x, m, a, s) can have an integer value ranging from a minimum of 1 to a maximum of 4000. Of all possible distinct combinations of ratings, your job is to figure out which ones will be accepted.

   In the above example, there are 167409079868000 distinct combinations of ratings that will be accepted.

   Consider only your list of workflows; the list of part ratings that the Elves wanted you to sort is no longer relevant. How many distinct combinations of ratings will be accepted by the Elves' workflows?
*)

(*
   Kudos to @EricKalkman for the solution. Here's a transcribed version using my datatypes.
   https://github.com/EricKalkman/AoC2023/blob/master/lib/day19.ml
*)

let count_paths rules =
  let rec count filters = function
    | Reject -> 0
    | Accept ->
        let range = Seq.ints 1 |> Seq.take 4000 in
        let counter cat =
          Seq.filter (CMap.find cat filters) range |> Seq.length
        in
        counter X * counter M * counter A * counter S
    | Workflow s ->
        let workflow = SMap.find s rules in
        let conditionals = List.filter (fun (c, _) -> c <> NA) workflow in
        let counts, elsefuncs =
          conditionals
          |> List.fold_left
               (fun (c, fs) (cond, r) ->
                 let cat, comp, i =
                   match cond with
                   | NA -> failwith "Impossible"
                   | Cond (cat, comp, i) -> (cat, comp, i)
                 in
                 let eval x = match comp with LT -> x < i | GT -> x > i in
                 let curr_fs =
                   CMap.update cat
                     (fun opt ->
                       match opt with
                       | None -> failwith "Impossible"
                       | Some fs' -> Some (fun x -> fs' x && eval x))
                     fs
                 in
                 let curr_count = count curr_fs r in
                 let next_fs =
                   CMap.update cat
                     (fun opt ->
                       match opt with
                       | None -> failwith "Impossible"
                       | Some fs' -> Some (fun x -> fs' x && (not @@ eval x)))
                     fs
                 in
                 (c + curr_count, next_fs))
               (0, filters)
        in
        let elsecount =
          count elsefuncs
            (List.filter (fun (c, _) -> c = NA) workflow |> List.hd |> snd)
        in
        counts + elsecount
  in
  let true_fn _ = true in
  let start =
    [ (X, true_fn); (M, true_fn); (A, true_fn); (S, true_fn) ] |> CMap.of_list
  in
  count start (Workflow "in")

let part_two file =
  let lines = file_lines file in
  let r, _ = split lines in
  count_paths @@ build_rules r
