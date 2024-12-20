open Core
open Advent.Matrix

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving show, equal, compare]

let next (coord : Advent.Matrix.coord) dir =
  let open Advent.Matrix in
  match dir with
  | Left -> { x = coord.x; y = coord.y - 1 }
  | Up -> { x = coord.x - 1; y = coord.y }
  | Right -> { x = coord.x; y = coord.y + 1 }
  | Down -> { x = coord.x + 1; y = coord.y }
;;

module Map = Stdlib.Map.Make (struct
    type t = Advent.Matrix.coord [@@deriving compare]
  end)

let dijkstra start matrix =
  let open Advent in
  let try_insert queue cost coord =
    match Matrix.get_opt matrix coord with
    | Some '.' | Some 'E' | Some 'S' -> Queue.enqueue queue (coord, cost)
    | _ -> ()
  in
  let rec aux queue map =
    match Queue.dequeue queue with
    | None -> map
    | Some (coord, cost) ->
      (match Map.find_opt coord map with
       | Some c when c <= cost -> aux queue map
       | _ ->
         let map = Map.add coord cost map in
         next coord Up |> try_insert queue (cost + 1);
         next coord Down |> try_insert queue (cost + 1);
         next coord Left |> try_insert queue (cost + 1);
         next coord Right |> try_insert queue (cost + 1);
         aux queue map)
  in
  let queue = Queue.create () in
  Queue.enqueue queue (start, 0);
  aux queue Map.empty
;;

let manhatten ca cb = abs (ca.x - cb.x) + abs (ca.y - cb.y)

let can_cheat amount (ca : Advent.Matrix.coord) (cb : Advent.Matrix.coord) =
  match manhatten ca cb with
  | x when x <= amount -> Some x
  | _ -> None
;;

let cheat amount min (path : (coord * int) list) =
  let can_cheat = can_cheat amount in
  List.fold path ~init:[] ~f:(fun acc (ca, na) ->
    List.fold path ~init:acc ~f:(fun acc (cb, nb) ->
      match can_cheat ca cb with
      | None -> acc
      | Some a ->
        let diff = na - nb in
        if diff <= 0
        then acc
        else (
          let diff = diff - a in
          if diff >= min then (ca, cb, diff) :: acc else acc)))
;;

let solve input amount min =
  let matrix =
    Advent.read_lines input
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun x -> x)
    |> Tuple3.get1
  in
  let start =
    Advent.Matrix.find_all matrix ~ch:[ 'S' ] ~comparer:Char.( = )
    |> List.hd_exn
    |> Tuple2.get2
  in
  dijkstra start matrix |> Map.to_list |> cheat amount min |> List.length
;;

let () = solve "inputs/day20-test.txt" 2 100 |> Fmt.pr "\nSample 1:%d"
let () = solve "inputs/day20-test.txt" 2 100 |> Fmt.pr "\nSample 2:%d"
let () = solve "inputs/day20.txt" 2 100 |> Fmt.pr "\nPart 1:%d"
let () = solve "inputs/day20.txt" 20 100 |> Fmt.pr "\nPart 2:%d"
