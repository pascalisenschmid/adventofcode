open Core

type coord = Advent.Matrix.coord [@@deriving compare]

module Map = Stdlib.Map.Make (struct
    type t = coord [@@deriving compare]
  end)

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

let dirs = function
  | Right -> [ Right, 1; Up, 1001; Down, 1001 ]
  | Up -> [ Up, 1; Left, 1001; Right, 1001 ]
  | Down -> [ Down, 1; Left, 1001; Right, 1001 ]
  | Left -> [ Left, 1; Up, 1001; Down, 1001 ]
;;

let get_next_opt matrix coord dir =
  let open Advent.Matrix in
  let next = next coord dir in
  match get_opt matrix next with
  | Some '.' | Some 'S' | Some 'E' -> Some next
  | _ -> None
;;

let dijkstra start dir matrix =
  let rec aux queue map =
    match Queue.dequeue queue with
    | None -> map
    | Some (coord, dir, cost) ->
      (match Map.find_opt coord map with
       | Some (c, _) when c <= cost -> aux queue map
       | _ ->
         let map = Map.add coord (cost, dir) map in
         dirs dir
         |> List.map ~f:(fun (dir, cost) ->
           get_next_opt matrix coord dir, dir, cost)
         |> List.filter ~f:(fun (coord_opt, _, _) -> Option.is_some coord_opt)
         |> List.map ~f:(fun (coord_opt, dir, add_cost) ->
           Option.value_exn coord_opt, dir, add_cost + cost)
         |> List.iter ~f:(fun item -> Queue.enqueue queue item);
         aux queue map)
  in
  let queue = Queue.create () in
  Queue.enqueue queue (start, dir, 0);
  aux queue Map.empty
;;

let solve input =
  let matrix =
    Advent.read_lines input
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun c -> c)
    |> Tuple3.get1
  in
  let start =
    Advent.Matrix.find_all matrix ~ch:[ 'S' ] ~comparer:Char.( = )
    |> List.hd_exn
    |> Tuple2.get2
  in
  let goal =
    Advent.Matrix.find_all matrix ~ch:[ 'E' ] ~comparer:Char.( = )
    |> List.hd_exn
    |> Tuple2.get2
  in
  let shortest_paths = dijkstra start Right matrix in
  let shortest_path, _ = shortest_paths |> Map.find goal in
  let part2 =
    Map.fold
      (fun coord (cost, dir) acc ->
         if cost > shortest_path
         then acc
         else (
           match dijkstra coord dir matrix |> Map.find_opt goal with
           | Some (ec, _) when ec + cost = shortest_path -> acc + 1
           | _ -> acc))
      shortest_paths
      1
  in
  sprintf "Part 1: %d" shortest_path |> print_endline;
  sprintf "Part 2: %d" part2 |> print_endline;
  ()
;;

let () = solve "inputs/day16-test.txt"
let () = solve "inputs/day16-test2.txt"
let () = solve "inputs/day16.txt"
