open Core

type coord = int * int [@@deriving compare, eq]

module CoordMap = Stdlib.Map.Make (struct
    type t = coord [@@deriving compare]
  end)

type direction =
  | U
  | D
  | L
  | R
  | S
[@@deriving compare]

module Nodes = Stdlib.Map.Make (struct
    type t = coord * coord [@@deriving compare]
  end)

let get_char (x, y) matrix =
  try Ok matrix.(y).(x) with
  | Failure e | Invalid_argument e -> Error e
;;

let next (x, y) dir =
  match dir with
  | U -> x, y - 1
  | D -> x, y + 1
  | R -> x + 1, y
  | L -> x - 1, y
  | S -> assert false
;;

let try_add_next matrix position visited dir out =
  let next = next position dir in
  let exists = CoordMap.find_opt next visited in
  match exists with
  | None ->
    (match get_char next matrix, dir with
     | Ok '^', U | Ok '>', R | Ok '<', L | Ok 'v', D | Ok '.', _ ->
       List.append out [ next, dir ]
     | _, _ -> out)
  | Some _ -> out
;;

let try_add_next2 matrix position visited dir out =
  let next = next position dir in
  let exists = CoordMap.find_opt next visited in
  match exists with
  | None ->
    (match get_char next matrix with
     | Ok '^' | Ok '>' | Ok '<' | Ok 'v' | Ok '.' ->
       List.append out [ next, dir ]
     | _ -> out)
  | Some _ -> out
;;

let get_walkable matrix position visited step_finder =
  let try_add = step_finder matrix position visited in
  try_add U [] |> try_add D |> try_add L |> try_add R
;;

let rec walk matrix position goal steps visited step_finder =
  let visited = CoordMap.add position true visited in
  if equal_coord position goal
  then steps
  else (
    let walkable = get_walkable matrix position visited step_finder in
    List.fold walkable ~init:0 ~f:(fun max (next, _) ->
      let r = walk matrix next goal (steps + 1) visited step_finder in
      Int.max r max))
;;

let create_graph matrix start goal =
  let rec aux start position steps visited nodes =
    let visited = CoordMap.add position true visited in
    if equal_coord position goal
    then Nodes.add (start, position) steps nodes, visited
    else (
      let walkable = get_walkable matrix position visited try_add_next in
      if List.length walkable = 1
      then aux start position (steps + 1) visited nodes
      else (
        let nodes = Nodes.add (start, position) steps nodes in
        List.fold
          walkable
          ~init:(nodes, visited)
          ~f:(fun (nodes, visited) (next, _) ->
            aux position next 0 visited nodes)))
  in
  aux start start 0 CoordMap.empty Nodes.empty
;;

let part1 matrix start goal =
  let max = walk matrix start goal 0 CoordMap.empty try_add_next in
  max |> sprintf "Part 1: %d" |> print_endline
;;

let part2 matrix start goal =
  let max = walk matrix start goal 0 CoordMap.empty try_add_next2 in
  max |> sprintf "Part 2: %d" |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day23.txt" |> List.to_array in
  let matrix, _, _ = Advent.char_matrix lines in
  let start = 1, 0 in
  let endx = lines.(0) |> String.length in
  let endy = Array.length lines in
  let goal = endx - 2, endy - 1 in
  (*let graph, _ = create_graph matrix start goal in
    Nodes.iter
    (fun ((sx, sy), (ex, ey)) stps ->
    sprintf "%d-%d -> %d-%d : %d" sx sy ex ey stps |> print_endline)
    graph*)
  let _ = part1 matrix start goal in
  part2 matrix start goal
;;
