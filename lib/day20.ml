(*
   --- Day 20: Pulse Propagation ---
   With your help, the Elves manage to find the right parts and fix all of the machines. Now, they just need to send the command to boot up the machines and get the sand flowing again.

   The machines are far apart and wired together with long cables. The cables don't connect to the machines directly, but rather to communication modules attached to the machines that perform various initialization tasks and also act as communication relays.

   Modules communicate using pulses. Each pulse is either a high pulse or a low pulse. When a module sends a pulse, it sends that type of pulse to each module in its list of destination modules.

   There are several different types of modules:

   Flip-flop modules (prefix %) are either on or off; they are initially off. If a flip-flop module receives a high pulse, it is ignored and nothing happens. However, if a flip-flop module receives a low pulse, it flips between on and off. If it was off, it turns on and sends a high pulse. If it was on, it turns off and sends a low pulse.

   Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules; they initially default to remembering a low pulse for each input. When a pulse is received, the conjunction module first updates its memory for that input. Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.

   There is a single broadcast module (named broadcaster). When it receives a pulse, it sends the same pulse to all of its destination modules.

   Here at Desert Machine Headquarters, there is a module with a single button on it called, aptly, the button module. When you push the button, a single low pulse is sent directly to the broadcaster module.

   After pushing the button, you must wait until all pulses have been delivered and fully handled before pushing it again. Never push the button if modules are still processing pulses.

   Pulses are always processed in the order they are sent. So, if a pulse is sent to modules a, b, and c, and then module a processes its pulse and sends more pulses, the pulses sent to modules b and c would have to be handled first.

   The module configuration (your puzzle input) lists each module. The name of the module is preceded by a symbol identifying its type, if any. The name is then followed by an arrow and a list of its destination modules. For example:

   broadcaster -> a, b, c
   %a -> b
   %b -> c
   %c -> inv
   &inv -> a
   In this module configuration, the broadcaster has three destination modules named a, b, and c. Each of these modules is a flip-flop module (as indicated by the % prefix). a outputs to b which outputs to c which outputs to another module named inv. inv is a conjunction module (as indicated by the & prefix) which, because it has only one input, acts like an inverter (it sends the opposite of the pulse type it receives); it outputs to a.

   By pushing the button once, the following pulses are sent:

   button -low-> broadcaster
   broadcaster -low-> a
   broadcaster -low-> b
   broadcaster -low-> c
   a -high-> b
   b -high-> c
   c -high-> inv
   inv -low-> a
   a -low-> b
   b -low-> c
   c -low-> inv
   inv -high-> a
   After this sequence, the flip-flop modules all end up off, so pushing the button again repeats the same sequence.

   Here's a more interesting example:

   broadcaster -> a
   %a -> inv, con
   &inv -> b
   %b -> con
   &con -> output
   This module configuration includes the broadcaster, two flip-flops (named a and b), a single-input conjunction module (inv), a multi-input conjunction module (con), and an untyped module named output (for testing purposes). The multi-input conjunction module con watches the two flip-flop modules and, if they're both on, sends a low pulse to the output module.

   Here's what happens if you push the button once:

   button -low-> broadcaster
   broadcaster -low-> a
   a -high-> inv
   a -high-> con
   inv -low-> b
   con -high-> output
   b -high-> con
   con -low-> output
   Both flip-flops turn on and a low pulse is sent to output! However, now that both flip-flops are on and con remembers a high pulse from each of its two inputs, pushing the button a second time does something different:

   button -low-> broadcaster
   broadcaster -low-> a
   a -low-> inv
   a -low-> con
   inv -high-> b
   con -high-> output
   Flip-flop a turns off! Now, con remembers a low pulse from module a, and so it sends only a high pulse to output.

   Push the button a third time:

   button -low-> broadcaster
   broadcaster -low-> a
   a -high-> inv
   a -high-> con
   inv -low-> b
   con -low-> output
   b -low-> con
   con -high-> output
   This time, flip-flop a turns on, then flip-flop b turns off. However, before b can turn off, the pulse sent to con is handled first, so it briefly remembers all high pulses for its inputs and sends a low pulse to output. After that, flip-flop b turns off, which causes con to update its state and send a high pulse to output.

   Finally, with a on and b off, push the button a fourth time:

   button -low-> broadcaster
   broadcaster -low-> a
   a -low-> inv
   a -low-> con
   inv -high-> b
   con -high-> output
   This completes the cycle: a turns off, causing con to remember only low pulses and restoring all modules to their original states.

   To get the cables warmed up, the Elves have pushed the button 1000 times. How many pulses got sent as a result (including the pulses sent by the button itself)?

   In the first example, the same thing happens every time the button is pushed: 8 low pulses and 4 high pulses are sent. So, after pushing the button 1000 times, 8000 low pulses and 4000 high pulses are sent. Multiplying these together gives 32000000.

   In the second example, after pushing the button 1000 times, 4250 low pulses and 2750 high pulses are sent. Multiplying these together gives 11687500.

   Consult your module configuration; determine the number of low pulses and high pulses that would be sent after pushing the button 1000 times, waiting for all pulses to be fully handled after each push of the button. What do you get if you multiply the total number of low pulses sent by the total number of high pulses sent?
*)

open Utils
module S = Map.Make (String)

type state = On | Off
type pulse = Low | High

type kind =
  | FlipFlop of state
  | Conjunction of pulse S.t
  | Broadcaster
  | Button

(* aka module *)
type modoole = { name : string; kind : kind; neighbors : string list }
type message = { sender : string; recipient : string; pulse : pulse }
type counts = { low : int; high : int }

let ( ++ ) c1 c2 = { low = c1.low + c2.low; high = c1.high + c2.high }
let button_name = "button"
let broadcaster_name = "broadcaster"

(* NOTE: this does not populate the map for conjunction modules *)
let parse_line line =
  match String.split_on_char ' ' line with
  | label :: "->" :: rest ->
      let neighbors =
        List.map (String.split_on_char ',') rest
        |> List.flatten
        |> List.filter (fun s -> s <> "")
      in
      let name, kind =
        match char_list_of_string label with
        | '%' :: name -> (List.to_seq name |> String.of_seq, FlipFlop Off)
        | '&' :: name -> (List.to_seq name |> String.of_seq, Conjunction S.empty)
        | _ -> (label, Broadcaster)
      in
      { name; kind; neighbors }
  | _ -> raise @@ Invalid_argument "Cannot parse line."

let update m k f =
  S.update k
    (fun opt ->
      match opt with
      | None ->
          raise @@ Invalid_argument (Printf.sprintf "Key '%s' not found!" k)
      | Some e -> Some (f e))
    m

let populate_conjunctions (modules : modoole S.t) =
  let mods = S.bindings modules |> List.map snd in
  List.fold_left
    (fun (acc : modoole S.t) (curr_mod : modoole) ->
      List.fold_left
        (fun (acc' : modoole S.t) neighbor ->
          match S.find neighbor acc' with
          | { kind = Conjunction np; name; _ } ->
              Printf.printf "UPDATING: name=%s, curr=%s, neighbor=%s\n%!" name
                curr_mod.name neighbor;
              let np' = S.add curr_mod.name Low np in
              update acc' name (fun r -> { r with kind = Conjunction np' })
          | _ -> acc')
        acc curr_mod.neighbors)
    modules mods

let rec process modules acc q =
  match q with
  | { sender; recipient; pulse } :: t -> (
      let counts =
        acc
        ++ {
             low = (if pulse = Low then 1 else 0);
             high = (if pulse = High then 1 else 0);
           }
      in
      Printf.printf "Searching for '%s'\n%!" recipient;
      let modoole = S.find recipient modules in
      match modoole.kind with
      | Button ->
          process modules counts
            (t
            @ [
                {
                  sender = modoole.name;
                  recipient = broadcaster_name;
                  pulse = Low;
                };
              ])
      | Broadcaster ->
          process modules counts
            (t
            @ List.map
                (fun recipient -> { sender = modoole.name; recipient; pulse })
                modoole.neighbors)
      | FlipFlop st ->
          if pulse = High then process modules counts t
          else (
            Printf.printf "updating\n%!";
            let modules' =
              update modules modoole.name (fun r ->
                  { r with kind = FlipFlop (if st = On then Off else On) })
            in
            process modules' counts
              (t
              @ List.map
                  (fun recipient ->
                    {
                      recipient;
                      pulse = (if st = Off then High else Low);
                      sender = modoole.name;
                    })
                  modoole.neighbors))
      | Conjunction np ->
          Printf.printf "Update conjunction for %s (sender=%s)\n%!" modoole.name
            sender;
          let np' = update np sender (fun _ -> pulse) in
          let modules' =
            update modules modoole.name (fun r ->
                { r with kind = Conjunction np' })
          in
          let pulse' =
            if S.bindings np' |> List.for_all (fun (_, p) -> p = High) then Low
            else High
          in
          process modules' counts
            (t
            @ List.map
                (fun recipient ->
                  { recipient; pulse = pulse'; sender = modoole.name })
                modoole.neighbors))
  | _ -> (modules, acc)

let press_button modules =
  process modules { low = 0; high = 0 }
    [ { sender = button_name; recipient = button_name; pulse = Low } ]

let part_one file =
  let lines = file_lines file in
  let modules =
    { name = button_name; kind = Button; neighbors = [ broadcaster_name ] }
    :: List.map parse_line lines
    |> List.map (fun r -> (r.name, r))
    |> S.of_list |> populate_conjunctions
  in
  Printf.printf "modules size=%d\n%!" (S.cardinal modules);
  let _, counts =
    List.fold_left
      (fun (modules', acc') _ ->
        let m'', acc'' = press_button modules' in
        (m'', acc' ++ acc''))
      (modules, { low = 0; high = 0 })
      (List.init 1000 Fun.id)
  in
  Printf.printf "low=%d \t high=%d\n%!" counts.low counts.high;
  counts.low * counts.high
