open Core

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving show, equal, compare]

let next (coord : Advent.Matrix.coordinate) = function
  | Left -> Advent.Matrix.{ x = coord.x; y = coord.y - 1 }
  | Up -> Advent.Matrix.{ x = coord.x - 1; y = coord.y }
  | Right -> Advent.Matrix.{ x = coord.x; y = coord.y + 1 }
  | Down -> Advent.Matrix.{ x = coord.x + 1; y = coord.y }
;;

module Set = Stdlib.Set.Make (struct
    type t = Advent.Matrix.coordinate [@@deriving compare]
  end)

let rec walk matrix current current_val set =
  let aux dir acc =
    let next_coord = next current dir in
    match Advent.Matrix.get_opt matrix next_coord with
    | Some x when x = current_val + 1 ->
      if x = 9 then Set.add next_coord acc else walk matrix next_coord x acc
    | _ -> acc
  in
  set |> aux Left |> aux Right |> aux Up |> aux Down
;;

let rec walk2 matrix current current_val =
  let aux dir =
    let next_coord = next current dir in
    match Advent.Matrix.get_opt matrix next_coord with
    | Some x when x = current_val + 1 ->
      if x = 9 then 1 else walk2 matrix next_coord x
    | _ -> 0
  in
  aux Left + aux Right + aux Up + aux Down
;;

let () =
  let matrix, _, _ =
    Advent.read_lines "inputs/day10.txt"
    |> Array.of_list
    |> Advent.Matrix.make ~f:Char.get_digit_exn
  in
  let starts =
    Advent.Matrix.find_all matrix ~ch:[ 0 ] ~comparer:Int.( = )
    |> List.map ~f:Tuple.T2.get2
  in
  let solve = List.fold starts ~init:0 in
  solve ~f:(fun acc coord ->
    acc + (walk matrix coord 0 Set.empty |> Set.cardinal))
  |> Fmt.pr "\nPart 1: %d";
  solve ~f:(fun acc coord -> acc + walk2 matrix coord 0)
  |> Fmt.pr "\nPart 2: %d"
;;
