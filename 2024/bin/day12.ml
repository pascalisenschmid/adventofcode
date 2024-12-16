open Core

module Seen = Stdlib.Set.Make (struct
    type t = Advent.Matrix.coord [@@deriving compare]
  end)

type direction =
  | Left
  | UpLeft
  | Up
  | UpRight
  | Right
  | DownRight
  | Down
  | DownLeft
[@@deriving show, equal, compare]

let next (coord : Advent.Matrix.coord) dir =
  let open Advent.Matrix in
  match dir with
  | Left -> { x = coord.x; y = coord.y - 1 }
  | UpLeft -> { x = coord.x - 1; y = coord.y - 1 }
  | Up -> { x = coord.x - 1; y = coord.y }
  | UpRight -> { x = coord.x - 1; y = coord.y + 1 }
  | Right -> { x = coord.x; y = coord.y + 1 }
  | DownRight -> { x = coord.x + 1; y = coord.y + 1 }
  | Down -> { x = coord.x + 1; y = coord.y }
  | DownLeft -> { x = coord.x + 1; y = coord.y - 1 }
;;

let has_neighbor matrix coord ch dir =
  match Advent.Matrix.get_opt matrix (next coord dir) with
  | Some c when Char.( = ) c ch -> true
  | _ -> false
;;

let check_perimeter matrix (coord : Advent.Matrix.coord) ch =
  let is_fence dir =
    match has_neighbor matrix coord ch dir with
    | true -> 0
    | false -> 1
  in
  is_fence Up + is_fence Right + is_fence Down + is_fence Left
;;

let check_corners matrix coord ch =
  let has_neighbor = has_neighbor matrix coord ch in
  let is_corner dir1 dir2 dir3 =
    match has_neighbor dir1, has_neighbor dir2, has_neighbor dir3 with
    | true, false, true -> 1
    | false, _, false -> 1
    | _ -> 0
  in
  is_corner Up UpRight Right
  + is_corner Right DownRight Down
  + is_corner Down DownLeft Left
  + is_corner Left UpLeft Up
;;

let rec walk matrix seen ch evaluator (coord : Advent.Matrix.coord) =
  match Advent.Matrix.get_opt matrix coord with
  | Some c when Char.( = ) c ch ->
    if Seen.mem coord seen
    then 0, 0, seen
    else (
      let seen = Seen.add coord seen in
      let perimeter = evaluator matrix coord ch in
      let next = next coord in
      [ next Up; next Down; next Right; next Left ]
      |> List.fold
           ~init:(perimeter, 1, seen)
           ~f:(fun (perimeter, area, seen) coord ->
             let p, a, seen = walk matrix seen ch evaluator coord in
             perimeter + p, area + a, seen))
  | _ -> 0, 0, seen
;;

let solve matrix evaluator =
  let list, _ =
    Array.foldi matrix ~init:([], Seen.empty) ~f:(fun idx acc row ->
      Array.foldi row ~init:acc ~f:(fun idy (acc, seen) ch ->
        let coord = Advent.Matrix.{ x = idx; y = idy } in
        let perimeter, area, seen = walk matrix seen ch evaluator coord in
        if area = 0 then acc, seen else (ch, area, perimeter) :: acc, seen))
  in
  list
;;

let () =
  let matrix, _, _ =
    Advent.read_lines "inputs/day12.txt"
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun x -> x)
  in
  solve matrix check_perimeter
  |> List.fold ~init:0 ~f:(fun acc (_, area, perimeter) ->
    acc + (area * perimeter))
  |> Fmt.pr "\nPart 1: %d";
  solve matrix check_corners
  |> List.fold ~init:0 ~f:(fun acc (_, area, perimeter) ->
    acc + (area * perimeter))
  |> Fmt.pr "\nPart 2: %d"
;;
