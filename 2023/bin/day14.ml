open Core
module Map = Stdlib.Map

type rock =
  | Round
  | Cube
  | Empty
[@@deriving eq, compare]

let parse_input lines =
  Advent.make_list_matrix lines ~f:(fun c ->
    match c with
    | 'O' -> Round
    | '#' -> Cube
    | _ -> Empty)
;;

let turn times (grid : rock list list) =
  let rec aux (grid : rock list list) times =
    match times with
    | 0 -> grid
    | 1 ->
      let grid = Array.of_list grid |> Array.map ~f:List.to_array in
      List.range 0 (Array.length grid) ~stop:`exclusive
      |> List.map ~f:(fun i ->
        Array.map grid ~f:(fun line -> line.(i)) |> List.of_array)
      |> List.rev
    | x -> aux (aux grid 1) (x - 1)
  in
  aux grid times
;;

let tilt pre post (grid : rock list list) =
  let grid = turn pre grid in
  let grid =
    List.map grid ~f:(fun line ->
      let split = Advent.split_opt line ~on:Cube ~equal:equal_rock in
      let rec aux groups acc =
        match groups with
        | [] -> acc
        | [ Some x ] ->
          let rounds = List.count x ~f:(fun rock -> equal_rock rock Round) in
          let acc =
            List.range 0 rounds ~stop:`exclusive
            |> List.fold ~init:acc ~f:(fun acc _ -> List.append acc [ Round ])
          in
          List.range 0 (List.length x - rounds) ~stop:`exclusive
          |> List.fold ~init:acc ~f:(fun acc _ -> List.append acc [ Empty ])
        | [ None ] -> List.append acc [ Cube ]
        | None :: rest ->
          let acc = List.append acc [ Cube ] in
          aux rest acc
        | x :: rest ->
          let acc = aux [ x ] acc in
          aux rest acc
      in
      aux split [])
  in
  turn post grid
;;

let calc_load grid =
  let rec aux grid weight acc =
    match grid with
    | [] -> acc
    | [ l ] ->
      let rounds = List.count l ~f:(fun rock -> equal_rock rock Round) in
      acc + (rounds * weight)
    | l :: rest -> aux rest (weight - 1) (aux [ l ] weight acc)
  in
  aux grid (List.length grid) 0
;;

let part1 grid =
  grid |> tilt 1 3 |> calc_load |> sprintf "Part 1: %d" |> print_endline
;;

module Mapp = Map.Make (struct
    type t = rock list list [@@deriving compare]
  end)

(*
   1
   2 - start
   3
   4
   5 - end/startover
   6
   7
   8 - cycle
   9
   10

   5 - 2 = length
   length % 3 = 2
   cycle 2 times
*)

let part2 grid =
  let cycle grid = grid |> tilt 1 3 |> tilt 0 0 |> tilt 3 1 |> tilt 2 2 in
  let rec aux count max map grid =
    let grid = cycle grid in
    let existing = Mapp.find_opt grid map in
    match existing with
    | Some x ->
      let cycle_length = count - x in
      let remaining = (max - count) mod cycle_length in
      grid, remaining
    | None when count < max ->
      let map = Mapp.add grid count map in
      aux (count + 1) max map grid
    | _ -> grid, 0
  in
  let grid, remaining = grid |> aux 1 1_000_000_000 Mapp.empty in
  let grid, _ = aux 1 remaining Mapp.empty grid in
  grid |> calc_load |> sprintf "Part 2: %d" |> print_endline
;;

let () =
  let _ = print_endline "" in
  let grid, _, _ =
    Advent.read_lines "./inputs/day14.txt" |> Array.of_list |> parse_input
  in
  let _ = part1 grid in
  part2 grid
;;
