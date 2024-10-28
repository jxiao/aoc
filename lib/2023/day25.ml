(*
   --- Day 25: Snowverload ---
   Still somehow without snow, you go to the last place you haven't checked: the center of Snow Island, directly below the waterfall.

   Here, someone has clearly been trying to fix the problem. Scattered everywhere are hundreds of weather machines, almanacs, communication modules, hoof prints, machine parts, mirrors, lenses, and so on.

   Somehow, everything has been wired together into a massive snow-producing apparatus, but nothing seems to be running. You check a tiny screen on one of the communication modules: Error 2023. It doesn't say what Error 2023 means, but it does have the phone number for a support line printed on it.

   "Hi, you've reached Weather Machines And So On, Inc. How can I help you?" You explain the situation.

   "Error 2023, you say? Why, that's a power overload error, of course! It means you have too many components plugged in. Try unplugging some components and--" You explain that there are hundreds of components here and you're in a bit of a hurry.

   "Well, let's see how bad it is; do you see a big red reset button somewhere? It should be on its own module. If you push it, it probably won't fix anything, but it'll report how overloaded things are." After a minute or two, you find the reset button; it's so big that it takes two hands just to get enough leverage to push it. Its screen then displays:

   SYSTEM OVERLOAD!

   Connected components would require
   power equal to at least 100 stars!
   "Wait, how many components did you say are plugged in? With that much equipment, you could produce snow for an entire--" You disconnect the call.

   You have nowhere near that many stars - you need to find a way to disconnect at least half of the equipment here, but it's already Christmas! You only have time to disconnect three wires.

   Fortunately, someone left a wiring diagram (your puzzle input) that shows how the components are connected. For example:

   jqt: rhn xhk nvd
   rsh: frs pzl lsr
   xhk: hfx
   cmg: qnr nvd lhk bvb
   rhn: xhk bvb hfx
   bvb: xhk hfx
   pzl: lsr hfx nvd
   qnr: nvd
   ntq: jqt hfx bvb xhk
   nvd: lhk
   lsr: lhk
   rzs: qnr cmg lsr rsh
   frs: qnr lhk lsr
   Each line shows the name of a component, a colon, and then a list of other components to which that component is connected. Connections aren't directional; abc: xyz and xyz: abc both represent the same configuration. Each connection between two components is represented only once, so some components might only ever appear on the left or right side of a colon.

   In this example, if you disconnect the wire between hfx/pzl, the wire between bvb/cmg, and the wire between nvd/jqt, you will divide the components into two separate, disconnected groups:

   9 components: cmg, frs, lhk, lsr, nvd, pzl, qnr, rsh, and rzs.
   6 components: bvb, hfx, jqt, ntq, rhn, and xhk.
   Multiplying the sizes of these groups together produces 54.

   Find the three wires you need to disconnect in order to divide the components into two separate groups. What do you get if you multiply the sizes of these two groups together?
*)

open Utils

module T = struct
  type t = string * string

  let compare = compare
end

module SM = Map.Make (String)
module SS = Set.Make (String)

let add_edges g v1 v2 =
  SM.update v1 (function None -> Some [ v2 ] | Some l -> Some (v2 :: l)) g
  |> SM.update v2 (function None -> Some [ v1 ] | Some l -> Some (v1 :: l))

let build_graph lines =
  let rec aux g = function
    | [] -> g
    | h :: t ->
        let g' =
          match String.split_on_char ':' h with
          | [ name; neighbors ] ->
              List.fold_left
                (fun acc s ->
                  if String.trim s <> "" then add_edges acc name (String.trim s)
                  else acc)
                g
                (String.split_on_char ' ' neighbors)
          | _ -> raise @@ Invalid_argument "Cannot parse line"
        in
        aux g' t
  in
  aux SM.empty lines

let set_minus_size s1 s2 = SS.cardinal s1 - (SS.inter s1 s2 |> SS.cardinal)

let part_one file =
  let lines = file_lines file in
  let graph = build_graph lines in
  let nodes = SM.bindings graph |> List.map fst |> SS.of_list in
  let g_set_minus_s = SM.bindings graph |> List.map fst |> SS.of_list in
  let count_neighbors_not_in_s s node =
    let g_v = SM.find node graph |> SS.of_list in
    set_minus_size g_v s
  in
  let num_uniq s =
    SS.fold (fun node acc -> count_neighbors_not_in_s s node + acc) s 0
  in
  let rec aux s =
    match num_uniq s with
    | 3 -> s
    | _ ->
        let to_remove =
          SS.fold
            (fun node (nacc, cacc) ->
              let nc = count_neighbors_not_in_s s node in
              if nc >= cacc then (node, nc) else (nacc, cacc))
            s ("", 0)
          |> fst
        in
        aux (SS.remove to_remove s)
  in
  let s' = aux g_set_minus_s in
  SS.cardinal s' * set_minus_size nodes s'
