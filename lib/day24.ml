(*
   --- Day 24: Never Tell Me The Odds ---
   It seems like something is going wrong with the snow-making process. Instead of forming snow, the water that's been absorbed into the air seems to be forming hail!

   Maybe there's something you can do to break up the hailstones?

   Due to strong, probably-magical winds, the hailstones are all flying through the air in perfectly linear trajectories. You make a note of each hailstone's position and velocity (your puzzle input). For example:

   19, 13, 30 @ -2,  1, -2
   18, 19, 22 @ -1, -1, -2
   20, 25, 34 @ -2, -2, -4
   12, 31, 28 @ -1, -2, -1
   20, 19, 15 @  1, -5, -3
   Each line of text corresponds to the position and velocity of a single hailstone. The positions indicate where the hailstones are right now (at time 0). The velocities are constant and indicate exactly how far each hailstone will move in one nanosecond.

   Each line of text uses the format px py pz @ vx vy vz. For instance, the hailstone specified by 20, 19, 15 @ 1, -5, -3 has initial X position 20, Y position 19, Z position 15, X velocity 1, Y velocity -5, and Z velocity -3. After one nanosecond, the hailstone would be at 21, 14, 12.

   Perhaps you won't have to do anything. How likely are the hailstones to collide with each other and smash into tiny ice crystals?

   To estimate this, consider only the X and Y axes; ignore the Z axis. Looking forward in time, how many of the hailstones' paths will intersect within a test area? (The hailstones themselves don't have to collide, just test for intersections between the paths they will trace.)

   In this example, look for intersections that happen with an X and Y position each at least 7 and at most 27; in your actual data, you'll need to check a much larger test area. Comparing all pairs of hailstones' future paths produces the following results:

   Hailstone A: 19, 13, 30 @ -2, 1, -2
   Hailstone B: 18, 19, 22 @ -1, -1, -2
   Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).

   Hailstone A: 19, 13, 30 @ -2, 1, -2
   Hailstone B: 20, 25, 34 @ -2, -2, -4
   Hailstones' paths will cross inside the test area (at x=11.667, y=16.667).

   Hailstone A: 19, 13, 30 @ -2, 1, -2
   Hailstone B: 12, 31, 28 @ -1, -2, -1
   Hailstones' paths will cross outside the test area (at x=6.2, y=19.4).

   Hailstone A: 19, 13, 30 @ -2, 1, -2
   Hailstone B: 20, 19, 15 @ 1, -5, -3
   Hailstones' paths crossed in the past for hailstone A.

   Hailstone A: 18, 19, 22 @ -1, -1, -2
   Hailstone B: 20, 25, 34 @ -2, -2, -4
   Hailstones' paths are parallel; they never intersect.

   Hailstone A: 18, 19, 22 @ -1, -1, -2
   Hailstone B: 12, 31, 28 @ -1, -2, -1
   Hailstones' paths will cross outside the test area (at x=-6, y=-5).

   Hailstone A: 18, 19, 22 @ -1, -1, -2
   Hailstone B: 20, 19, 15 @ 1, -5, -3
   Hailstones' paths crossed in the past for both hailstones.

   Hailstone A: 20, 25, 34 @ -2, -2, -4
   Hailstone B: 12, 31, 28 @ -1, -2, -1
   Hailstones' paths will cross outside the test area (at x=-2, y=3).

   Hailstone A: 20, 25, 34 @ -2, -2, -4
   Hailstone B: 20, 19, 15 @ 1, -5, -3
   Hailstones' paths crossed in the past for hailstone B.

   Hailstone A: 12, 31, 28 @ -1, -2, -1
   Hailstone B: 20, 19, 15 @ 1, -5, -3
   Hailstones' paths crossed in the past for both hailstones.
   So, in this example, 2 hailstones' future paths cross inside the boundaries of the test area.

   However, you'll need to search a much larger test area if you want to see if any hailstones might collide. Look for intersections that happen with an X and Y position each at least 200000000000000 and at most 400000000000000. Disregard the Z axis entirely.

   Considering only the X and Y axes, check all pairs of hailstones' future paths for intersections. How many of these intersections occur within the test area?
*)

open Utils

type hailstone = {
  x : float;
  y : float;
  z : float;
  vx : float;
  vy : float;
  vz : float;
}

let parse line =
  let get_float s = float_of_string @@ String.trim s in
  match String.split_on_char '@' line with
  | [ position; velocity ] -> (
      match
        (String.split_on_char ',' position, String.split_on_char ',' velocity)
      with
      | [ x; y; z ], [ vx; vy; vz ] ->
          {
            x = get_float x;
            y = get_float y;
            z = get_float z;
            vx = get_float vx;
            vy = get_float vy;
            vz = get_float vz;
          }
      | _, _ -> raise @@ Invalid_argument ("Cannot parse line " ^ line))
  | _ -> raise @@ Invalid_argument ("Cannot parse line " ^ line)

let slope { vx; vy; _ } = vy /. vx

type parity = Pos | Neg | Zero

let get_parity v = if v = 0. then Zero else if v < 0. then Neg else Pos

let intersection ({ x = x1; y = y1; vx = vx1; vy = vy1; _ } as h1)
    ({ x = x2; y = y2; vx = vx2; vy = vy2; _ } as h2) =
  let m1, m2 = (slope h1, slope h2) in
  if m1 = m2 then None
  else
    let x = ((m1 *. x1) -. (m2 *. x2) +. y2 -. y1) /. (m1 -. m2) in
    let y = (m1 *. (x -. x1)) +. y1 in
    if
      get_parity (x -. x1) = get_parity vx1
      && get_parity (y -. y1) = get_parity vy1
      && get_parity (x -. x2) = get_parity vx2
      && get_parity (y -. y2) = get_parity vy2
    then Some (x, y)
    else None

let pairs l =
  snd
  @@ List.fold_left
       (fun (i, acc1) v1 ->
         ( i + 1,
           acc1 @ List.rev
           @@ snd
                (List.fold_left
                   (fun (j, acc2) v2 ->
                     (j + 1, if j <= i then acc2 else (v1, v2) :: acc2))
                   (0, []) l) ))
       (0, []) l

let part_one file =
  let lines = file_lines file in
  let stones = List.map parse lines in
  let stone_pairs = pairs stones in
  let intersections =
    List.map (fun (h1, h2) -> intersection h1 h2) stone_pairs
  in
  let lower, upper = (200000000000000., 400000000000000.) in
  let within_test_area =
    List.filter
      (function
        | None -> false
        | Some (x, y) -> x >= lower && y >= lower && x <= upper && y <= upper)
      intersections
  in
  List.length within_test_area

(*
   --- Part Two ---
   Upon further analysis, it doesn't seem like any hailstones will naturally collide. It's up to you to fix that!

   You find a rock on the ground nearby. While it seems extremely unlikely, if you throw it just right, you should be able to hit every hailstone in a single throw!

   You can use the probably-magical winds to reach any integer position you like and to propel the rock at any integer velocity. Now including the Z axis in your calculations, if you throw the rock at time 0, where do you need to be so that the rock perfectly collides with every hailstone? Due to probably-magical inertia, the rock won't slow down or change direction when it collides with a hailstone.

   In the example above, you can achieve this by moving to position 24, 13, 10 and throwing the rock at velocity -3, 1, 2. If you do this, you will hit every hailstone as follows:

   Hailstone: 19, 13, 30 @ -2, 1, -2
   Collision time: 5
   Collision position: 9, 18, 20

   Hailstone: 18, 19, 22 @ -1, -1, -2
   Collision time: 3
   Collision position: 15, 16, 16

   Hailstone: 20, 25, 34 @ -2, -2, -4
   Collision time: 4
   Collision position: 12, 17, 18

   Hailstone: 12, 31, 28 @ -1, -2, -1
   Collision time: 6
   Collision position: 6, 19, 22

   Hailstone: 20, 19, 15 @ 1, -5, -3
   Collision time: 1
   Collision position: 21, 14, 12
   Above, each hailstone is identified by its initial position and its velocity. Then, the time and position of that hailstone's collision with your rock are given.

   After 1 nanosecond, the rock has exactly the same position as one of the hailstones, obliterating it into ice dust! Another hailstone is smashed to bits two nanoseconds after that. After a total of 6 nanoseconds, all of the hailstones have been destroyed.

   So, at time 0, the rock needs to be at X position 24, Y position 13, and Z position 10. Adding these three coordinates together produces 47. (Don't add any coordinates from the rock's velocity.)

   Determine the exact position and velocity the rock needs to have at time 0 so that it perfectly collides with every hailstone. What do you get if you add up the X, Y, and Z coordinates of that initial position?
*)

(* Linear algebra solution courtesy of: https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/kxqjg33/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button *)

let scalar k (u1, u2, u3) = (k *. u1, k *. u2, k *. u3)
let div k (u1, u2, u3) = (u1 /. k, u2 /. k, u3 /. k)
let add (u1, u2, u3) (v1, v2, v3) = (u1 +. v1, u2 +. v2, u3 +. v3)
let sub p1 p2 = add p1 (scalar (-1.) p2)

let cross (u1, u2, u3) (v1, v2, v3) =
  ((u2 *. v3) -. (u3 *. v2), (u3 *. v1) -. (u1 *. v3), (u1 *. v2) -. (u2 *. v1))

let dot (u1, u2, u3) (v1, v2, v3) = (u1 *. v1) +. (u2 *. v2) +. (u3 *. v3)
let vectors h = ((h.x, h.y, h.z), (h.vx, h.vy, h.vz))

let part_two file =
  let lines = file_lines file in
  let stones = List.map parse lines in
  let (pos0, vel0), (pos1, vel1), (pos2, vel2) =
    match List.to_seq stones |> Seq.take 3 |> List.of_seq with
    | [ h0; h1; h2 ] -> (vectors h0, vectors h1, vectors h2)
    | _ -> failwith "Impossible"
  in
  let p1, v1, p2, v2 =
    (sub pos1 pos0, sub vel1 vel0, sub pos2 pos0, sub vel2 vel0)
  in
  let t1 = -1. *. (cross p1 p2 |> dot v2) /. (cross v1 p2 |> dot v2) in
  let t2 = -1. *. (cross p1 p2 |> dot v1) /. (cross p1 v2 |> dot v1) in
  let c1, c2 = (add pos1 (scalar t1 vel1), add pos2 (scalar t2 vel2)) in
  let v = div (t2 -. t1) (sub c2 c1) in
  let x, y, z = sub c1 (scalar t1 v) in
  Float.round x +. Float.round y +. Float.round z |> int_of_float
