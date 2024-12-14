open Core

type robot =
  { px : int
  ; py : int
  ; vx : int
  ; vy : int
  }
[@@deriving show]

module Set = Stdlib.Set.Make (struct
    type t = int * int [@@deriving compare]
  end)

let parse_robot line =
  let split = String.split_on_chars line ~on:[ '='; ','; ' ' ] in
  let px = List.nth_exn split 1 |> Int.of_string in
  let py = List.nth_exn split 2 |> Int.of_string in
  let vx = List.nth_exn split 4 |> Int.of_string in
  let vy = List.nth_exn split 5 |> Int.of_string in
  { px; py; vx; vy }
;;

let simulate steps xmax ymax robot =
  let x = (robot.px + (robot.vx * steps)) % xmax in
  let y = (robot.py + (robot.vy * steps)) % ymax in
  { px = x; py = y; vx = robot.vx; vy = robot.vy }
;;

let eval_quadrant xmax ymax (q1, q2, q3, q4) robot =
  let x_div = xmax / 2 in
  let y_div = ymax / 2 in
  match robot with
  | { px; py; _ } when px < x_div && py < y_div -> q1 + 1, q2, q3, q4
  | { px; py; _ } when px > x_div && py < y_div -> q1, q2 + 1, q3, q4
  | { px; py; _ } when px < x_div && py > y_div -> q1, q2, q3 + 1, q4
  | { px; py; _ } when px > x_div && py > y_div -> q1, q2, q3, q4 + 1
  | _ -> q1, q2, q3, q4
;;

let muliply_quadrants (q1, q2, q3, q4) = q1 * q2 * q3 * q4

let count_unique_robots robots =
  List.fold robots ~init:Set.empty ~f:(fun set robot ->
    Set.add (robot.px, robot.py) set)
  |> Set.cardinal
;;

let part1 robots =
  let simulate = simulate 100 101 103 in
  let eval_quadrant = eval_quadrant 101 103 in
  List.map robots ~f:simulate
  |> List.fold ~init:(0, 0, 0, 0) ~f:eval_quadrant
  |> muliply_quadrants
;;

(*let print_robots robots xmax ymax =*)
(*  let matrix = Array.make_matrix ~dimx:ymax ~dimy:ymax '.' in*)
(*  let matrix =*)
(*    List.fold robots ~init:matrix ~f:(fun m robot ->*)
(*      m.(robot.py).(robot.px) <- 'X';*)
(*      m)*)
(*  in*)
(*  Array.iter matrix ~f:(fun row ->*)
(*    Fmt.pr "\n";*)
(*    Array.iter row ~f:(fun c -> Fmt.pr "%c" c))*)
(*;;*)

let part2 robots =
  let simulate = simulate 1 101 103 in
  let robots_count = List.length robots in
  let rec simulate_next robots round =
    let robots = List.map robots ~f:simulate in
    let count = count_unique_robots robots in
    match robots_count = count with
    | true ->
      (*print_robots robots 101 103;*)
      round
    | false -> simulate_next robots (round + 1)
  in
  simulate_next robots 1
;;

let () =
  let robots =
    Advent.read_lines "inputs/day14.txt" |> List.map ~f:parse_robot
  in
  part1 robots |> Fmt.pr "\nPart 1:%d";
  part2 robots |> Fmt.pr "\nPart 2:%d"
;;
