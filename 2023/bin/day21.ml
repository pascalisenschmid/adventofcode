open Core
module Map = Stdlib.Map

module Nodes = Map.Make (struct
    type t = int * int [@@deriving compare]
  end)

let parse_input lines =
  let x = lines.(0) |> String.to_array |> Array.length in
  let y = Array.length lines in
  let start = ref (0, 0) in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun y row ->
      let line = lines.(y) |> String.to_array in
      Array.mapi row ~f:(fun x _ ->
        let c = line.(x) in
        match c with
        | 'S' ->
          start := x, y;
          c
        | _ -> c))
  in
  map, !start
;;

let get_node (x, y) matrix =
  try Ok matrix.(y).(x) with
  | Failure _ | Invalid_argument _ -> Error "notfound"
;;

let directions (x, y) = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

let create_visited (matrix, start) =
  let rec aux queue nodes inqueue =
    let coord = Queue.dequeue queue in
    match coord with
    | None -> nodes
    | Some (coord : int * (int * int)) ->
      let step, coord = coord in
      let nodes = Nodes.add coord step nodes in
      let inqueue =
        List.fold
          (directions coord)
          ~init:inqueue
          ~f:(fun inqueue (new_coord : int * int) ->
            match get_node new_coord matrix with
            | Ok '.' | Ok 'S' ->
              (match
                 ( Nodes.find_opt new_coord nodes
                 , Nodes.find_opt new_coord inqueue )
               with
               | Some _, _ -> inqueue
               | None, None ->
                 let _ = Queue.enqueue queue (step + 1, new_coord) in
                 Nodes.add new_coord true inqueue
               | _ -> inqueue)
            | _ -> inqueue)
      in
      aux queue nodes inqueue
  in
  let queue = Queue.create () in
  let _ = Queue.enqueue queue (0, start) in
  aux queue Nodes.empty Nodes.empty
;;

let part1 visited =
  visited
  |> Nodes.filter (fun _ v -> v % 2 = 0 && v <= 64)
  |> Nodes.cardinal
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let part2 visited =
  let even_corners =
    Nodes.filter (fun _ v -> v % 2 = 0 && v > 65) visited |> Nodes.cardinal
  in
  let odd_corners =
    Nodes.filter (fun _ v -> v % 2 = 1 && v > 65) visited |> Nodes.cardinal
  in
  let even_full =
    Nodes.filter (fun _ v -> v % 2 = 0) visited |> Nodes.cardinal
  in
  let odd_full =
    Nodes.filter (fun _ v -> v % 2 = 1) visited |> Nodes.cardinal
  in
  let n = (26501365 - (131 / 2)) / 131 in
  let result =
    ((n + 1) * (n + 1) * odd_full)
    + (n * n * even_full)
    - ((n + 1) * odd_corners)
    + (n * even_corners)
  in
  sprintf "Part 2: %d" result |> print_endline
;;

let () =
  let visited =
    Advent.read_lines "./inputs/day21.txt"
    |> List.to_array
    |> parse_input
    |> create_visited
  in
  let _ = part1 visited in
  part2 visited
;;
