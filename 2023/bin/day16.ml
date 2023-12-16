open Core
module Map = Stdlib.Map

module Visited = Map.Make (struct
    type t = int * int [@@deriving compare]
  end)

type direction =
  | Up
  | Down
  | Left
  | Right
[@@deriving eq]

let next (x, y) dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Right -> x + 1, y
  | Left -> x - 1, y
;;

let get_char_opt (x, y) matrix =
  try Some matrix.(y).(x) with
  | Failure _ | Invalid_argument _ -> None
;;

let rec solve coord dir visited matrix =
  let aux dir coord curr visited matrix =
    match dir, curr with
    | _, '.' -> solve (next coord dir) dir visited matrix
    | Right, '-' | Left, '-' | Up, '|' | Down, '|' ->
      solve (next coord dir) dir visited matrix
    | Left, '/' | Right, '\\' -> solve (next coord Down) Down visited matrix
    | Left, '\\' | Right, '/' -> solve (next coord Up) Up visited matrix
    | Up, '/' | Down, '\\' -> solve (next coord Right) Right visited matrix
    | Up, '\\' | Down, '/' -> solve (next coord Left) Left visited matrix
    | Left, '|' | Right, '|' ->
      let visited = solve (next coord Up) Up visited matrix in
      solve (next coord Down) Down visited matrix
    | Up, '-' | Down, '-' ->
      let visited = solve (next coord Right) Right visited matrix in
      solve (next coord Left) Left visited matrix
    | _, _ -> visited
  in
  match get_char_opt coord matrix with
  | None -> visited
  | Some curr ->
    (match Visited.find_opt coord visited with
     | Some dirs
       when Option.is_some (List.find dirs ~f:(fun d -> equal_direction d dir))
       -> visited
     | Some dirs ->
       let dirs = List.append dirs [ dir ] in
       let visited = Visited.add coord dirs visited in
       aux dir coord curr visited matrix
     | None ->
       let visited = Visited.add coord [ dir ] visited in
       aux dir coord curr visited matrix)
;;

let part1 matrix start dir =
  matrix |> solve start dir Visited.empty |> Visited.cardinal
;;

let part2 matrix xmax ymax =
  let res =
    List.range 0 xmax
    |> List.fold ~init:0 ~f:(fun acc x ->
      let hit = part1 matrix (x, 0) Down in
      Int.max acc hit)
  in
  let res =
    List.range 0 xmax
    |> List.fold ~init:res ~f:(fun acc x ->
      let hit = part1 matrix (x, ymax - 1) Up in
      Int.max acc hit)
  in
  let res =
    List.range 0 ymax
    |> List.fold ~init:res ~f:(fun acc y ->
      let hit = part1 matrix (0, y) Right in
      Int.max acc hit)
  in
  List.range 0 ymax
  |> List.fold ~init:res ~f:(fun acc y ->
    let hit = part1 matrix (xmax - 1, y) Left in
    Int.max acc hit)
;;

let () =
  let matrix, x, y =
    Advent.read_lines "./inputs/day16.txt"
    |> Array.of_list
    |> Advent.char_matrix
  in
  let _ = part1 matrix (0, 0) Right |> sprintf "Part 1: %d" |> print_endline in
  part2 matrix x y |> sprintf "Part 2: %d" |> print_endline
;;
