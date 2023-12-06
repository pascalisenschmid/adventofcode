open Core

module Race = struct
  type t =
    { time : int
    ; distance : int
    }

  let create nums =
    let races =
      List.map2 (List.hd_exn nums) (List.last_exn nums) ~f:(fun t d ->
        { time = t; distance = d })
    in
    match races with
    | Ok list -> list
    | _ -> assert false
  ;;
end

open Race

let rec parse lines out (aux : string -> int list) =
  match lines with
  | [] -> out
  | [ line ] ->
    let nums = aux line in
    parse [] (out @ [ nums ]) aux
  | line :: rest ->
    let nums = aux line in
    parse rest (out @ [ nums ]) aux
;;

let part1parser line =
  String.split line ~on:' '
  |> List.filter ~f:Advent.string_is_int
  |> List.map ~f:Int.of_string
;;

let part2parser line =
  [ String.split line ~on:' '
    |> List.filter ~f:Advent.string_is_int
    |> String.concat
    |> Int.of_string
  ]
;;

let solve round race = round * (race.time - round) > race.distance

let solve race =
  let rec second race sqrt current first_found start =
    match solve current race with
    | true when first_found -> start, current
    | true -> second race sqrt (current + sqrt - 1) true start
    | false when first_found -> second race sqrt (current - 1) true start
    | false -> second race sqrt (current - sqrt) false start
  in
  let rec first race sqrt current (first_found : bool) =
    match solve current race with
    | true when first_found -> second race sqrt (race.time - sqrt) false current
    | true -> first race sqrt (current - sqrt + 1) true
    | false when first_found -> first race sqrt (current + 1) first_found
    | false -> first race sqrt (current + sqrt) first_found
  in
  let sqrt =
    Int.to_float race.time |> Float.sqrt |> Float.round_down |> Int.of_float
  in
  let start, finish = first race sqrt sqrt false in
  finish - start + 1
;;

let solve_part_1 lines =
  let nums = parse lines [] part1parser in
  let races = Race.create nums in
  let res = List.fold races ~init:1 ~f:(fun acc race -> acc * solve race) in
  sprintf "Part 1: %d" res |> print_endline
;;

let solve_part_2 lines =
  let nums = parse lines [] part2parser in
  let races = Race.create nums in
  let res = List.fold races ~init:1 ~f:(fun _ race -> solve race) in
  sprintf "Part 2: %d" res |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day6.txt" in
  let _ = solve_part_1 lines in
  solve_part_2 lines
;;
