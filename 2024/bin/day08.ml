open Core

module Set = Stdlib.Set.Make (struct
    type t = Advent.Matrix.coordinate [@@deriving compare]
  end)

let distance (a : Advent.Matrix.coordinate) (b : Advent.Matrix.coordinate) =
  a.x - b.x, a.y - b.y
;;

let positive = Int.( + )
let negative = Int.( - )

let move (dx, dy) direction coord =
  Advent.Matrix.{ x = direction coord.x dx; y = direction coord.y dy }
;;

let part1 in_bounds coord other_nodes set =
  let add_if_in_bounds set coord =
    match in_bounds coord with
    | true -> Set.add coord set
    | false -> set
  in
  List.fold other_nodes ~init:set ~f:(fun acc other ->
    let move = move (distance coord other) in
    let coord = move positive coord in
    let acc = add_if_in_bounds acc coord in
    let coord = move negative other in
    let acc = add_if_in_bounds acc coord in
    acc)
;;

let part2 in_bounds coord other_nodes set =
  List.fold other_nodes ~init:set ~f:(fun acc other ->
    let move = move (distance coord other) in
    let rec move_til_oob set move coord =
      let antinode = move coord in
      match in_bounds antinode with
      | true ->
        let set = Set.add antinode set in
        move_til_oob set move antinode
      | false -> set
    in
    let acc = move_til_oob acc (move positive) coord in
    let acc = move_til_oob acc (move positive) other in
    let acc = move_til_oob acc (move negative) coord in
    let acc = move_til_oob acc (move negative) other in
    acc)
;;

let in_bounds max_x max_y = function
  | Advent.Matrix.{ x; y } when x >= 0 && x < max_x && y >= 0 && y < max_y ->
    true
  | _ -> false
;;

let solve antennas in_bounds part_solver =
  List.fold antennas ~init:Set.empty ~f:(fun acc group ->
    let rec aux group acc =
      match group with
      | [] | [ _ ] -> acc
      | hd :: rest ->
        let acc = part_solver in_bounds hd rest acc in
        aux rest acc
    in
    aux group acc)
  |> Set.cardinal
;;

let () =
  let matrix, x, y =
    Advent.read_lines "inputs/day08.txt"
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun x -> x)
  in
  let in_bounds = in_bounds x y in
  let antennas =
    Advent.Matrix.find_all_except ~not:[ '.' ] ~comparer:Char.( = ) matrix
    |> List.sort_and_group ~compare:(fun (a, _) (b, _) -> Char.compare a b)
    |> List.map ~f:(fun group -> List.map group ~f:(fun (_, c) -> c))
  in
  let solve = solve antennas in_bounds in
  let _ = solve part1 |> Fmt.pr "Part 1: %d\n" in
  solve part2 |> Fmt.pr "Part 2: %d\n"
;;
