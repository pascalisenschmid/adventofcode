open Core
module Map = Stdlib.Map

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving compare]

type coord = int * int [@@deriving eq, compare]

module Visited = Map.Make (struct
    type t = coord * direction * int [@@deriving compare]
  end)

let next (x, y) dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Right -> x + 1, y
  | Left -> x - 1, y
;;

let get_int_opt (x, y) matrix =
  try Some matrix.(y).(x) with
  | Failure _ | Invalid_argument _ -> None
;;

let right coord count heat queue =
  let _ = Queue.enqueue queue (next coord Right, Right, count, heat) in
  queue
;;

let left coord count heat queue =
  let _ = Queue.enqueue queue (next coord Left, Left, count, heat) in
  queue
;;

let up coord count heat queue =
  let _ = Queue.enqueue queue (next coord Up, Up, count, heat) in
  queue
;;

let down coord count heat queue =
  let _ = Queue.enqueue queue (next coord Down, Down, count, heat) in
  queue
;;

let rec part1 matrix goal queue visited min_heat =
  match Queue.dequeue queue with
  | None -> min_heat
  | Some (coord, dir, count, heat') ->
    (match get_int_opt coord matrix with
     | None -> part1 matrix goal queue visited min_heat
     | Some heat when equal_coord coord goal ->
       let heat = heat + heat' in
       let _ = sprintf "final heat %d" heat |> print_endline in
       (match Visited.find_opt (coord, dir, count) visited with
        | None ->
          let visited = Visited.add (coord, dir, count) heat visited in
          part1 matrix goal queue visited heat
        | Some vis when vis > heat ->
          let visited = Visited.add (coord, dir, count) heat visited in
          part1 matrix goal queue visited heat
        | Some vis -> part1 matrix goal queue visited vis)
     | Some heat ->
       let aux coord dir count heat queue visited =
         let _ =
           match dir, count with
           | Left, 3 | Right, 3 ->
             queue |> right coord 1 heat |> left coord 1 heat
           | Up, 3 | Down, 3 -> queue |> up coord 1 heat |> down coord 1 heat
           | Right, c ->
             queue
             |> up coord 1 heat
             |> down coord 1 heat
             |> right coord (c + 1) heat
           | Left, c ->
             queue
             |> up coord 1 heat
             |> down coord 1 heat
             |> left coord (c + 1) heat
           | Up, c ->
             queue
             |> right coord 1 heat
             |> left coord 1 heat
             |> up coord (c + 1) heat
           | Down, c ->
             queue
             |> left coord 1 heat
             |> right coord 1 heat
             |> down coord (c + 1) heat
         in
         part1 matrix goal queue visited min_heat
       in
       let heat = heat + heat' in
       (match heat with
        | heat when heat > min_heat -> part1 matrix goal queue visited min_heat
        | heat ->
          (match Visited.find_opt (coord, dir, count) visited with
           | None ->
             let visited = Visited.add (coord, dir, count) heat visited in
             aux coord dir count heat queue visited
           | Some vis when vis > heat ->
             let visited = Visited.add (coord, dir, count) heat visited in
             aux coord dir count heat queue visited
           | Some _ -> part1 matrix goal queue visited min_heat)))
;;

(*| None -> min_heat, visited
  | Some heat ->
  (match curr with
  | pos when equal_coord goal pos ->
  Int.min (heat_acc + heat) min_heat, visited
  | pos ->
  let heat_acc = heat_acc + heat in
  (match heat_acc with
  | heat_acc when heat_acc >= min_heat -> min_heat, visited
  | heat_acc ->
  let walking visited =
  let r = part1 matrix (next pos Right) goal Right heat_acc in
  let l = part1 matrix (next pos Left) goal Left heat_acc in
  let u = part1 matrix (next pos Up) goal Up heat_acc in
  let d = part1 matrix (next pos Down) goal Down heat_acc in
  match dir, same_dir_count with
  | Left, 3 | Right, 3 -> (min_heat, visited) |> u 1 |> d 1
  | Up, 3 | Down, 3 -> (min_heat, visited) |> r 1 |> l 1
  | Right, x when x < 3 ->
  (min_heat, visited) |> u 1 |> d 1 |> r (x + 1)
  | Left, x when x < 3 ->
  (min_heat, visited) |> u 1 |> d 1 |> l (x + 1)
  | Up, x when x < 3 ->
  (min_heat, visited) |> l 1 |> r 1 |> u (x + 1)
  | Down, x when x < 3 ->
  (min_heat, visited) |> l 1 |> r 1 |> d (x + 1)
  | _ -> assert false
  in
  (match Visited.find_opt (pos, dir, same_dir_count) visited with
  | None ->
  let visited =
  Visited.add (pos, dir, same_dir_count) heat_acc visited
  in
  walking visited
  | Some best when best <= heat_acc -> min_heat, visited
  | Some _ ->
  let visited =
  Visited.add (pos, dir, same_dir_count) heat_acc visited
  in
  walking visited))))

  let rec part2
  matrix
  curr
  goal
  dir
  heat_acc
  same_dir_count
  queue
  (min_heat, visited)
  =
  match get_int_opt curr matrix, curr, same_dir_count with
  | None, _, _ -> min_heat, visited
  | Some heat, pos, x when equal_coord goal pos && x >= 4 ->
  Int.min (heat_acc + heat) min_heat, visited
  | Some _, pos, x when equal_coord goal pos && x < 4 -> min_heat, visited
  | Some heat, pos, _ ->
  let heat_acc = heat_acc + heat in
  (match heat_acc with
  | heat_acc when heat_acc >= min_heat -> min_heat, visited
  | heat_acc ->
  let walking visited =
  let r = part2 matrix (next pos Right) goal Right heat_acc in
  let l = part2 matrix (next pos Left) goal Left heat_acc in
  let u = part2 matrix (next pos Up) goal Up heat_acc in
  let d = part2 matrix (next pos Down) goal Down heat_acc in
  match dir, same_dir_count with
  | Left, 10 | Right, 10 -> (min_heat, visited) |> u 1 |> d 1
  | Up, 10 | Down, 10 -> (min_heat, visited) |> r 1 |> l 1
  | Left, x when x <= 3 -> (min_heat, visited) |> l (x + 1)
  | Right, x when x <= 3 -> (min_heat, visited) |> r (x + 1)
  | Up, x when x <= 3 -> (min_heat, visited) |> u (x + 1)
  | Down, x when x <= 3 -> (min_heat, visited) |> d (x + 1)
  | Right, x -> (min_heat, visited) |> u 1 |> d 1 |> r (x + 1)
  | Left, x -> (min_heat, visited) |> u 1 |> d 1 |> l (x + 1)
  | Up, x -> (min_heat, visited) |> l 1 |> r 1 |> u (x + 1)
  | Down, x -> (min_heat, visited) |> l 1 |> r 1 |> d (x + 1)
  in
  (match Visited.find_opt (pos, dir, same_dir_count) visited with
  | None ->
  let visited =
  Visited.add (pos, dir, same_dir_count) heat_acc visited
  in
  walking visited
  | Some best when best <= heat_acc -> min_heat, visited
  | Some _ ->
  let visited =
  Visited.add (pos, dir, same_dir_count) heat_acc visited
  in
  walking visited))
  ;;*)

let walk part matrix start goal =
  let queue = Queue.create () in
  let _ = Queue.enqueue queue (next start Right, Right, 1, 0) in
  let _ = Queue.enqueue queue (next start Down, Down, 1, 0) in
  part matrix goal queue Visited.empty Int.max_value
;;

(*let visited = part matrix goal queue Visited.empty Int.max_value in
  Visited.filter (fun (c, _, _) _ -> equal_coord c goal) visited
  |> Visited.to_list
  |> List.fold ~init:Int.max_value ~f:(fun acc (_, v) -> Int.min acc v)*)

let () =
  let matrix, x, y =
    Advent.read_lines "./inputs/day17.txt"
    |> List.to_array
    |> Advent.make_matrix_array ~f:(fun char ->
      char |> String.of_char |> Int.of_string)
  in
  let _ =
    walk part1 matrix (0, 0) (x - 1, y - 1)
    |> sprintf "Part 1: %d"
    |> print_endline
  in
  ()
;;
(*walk part2 matrix (0, 0) (x - 1, y - 1) ((x * 9) + (y * 9))
  |> sprintf "Part 2: %d"
  |> print_endline*)
