open Core

type direction =
  | U
  | D
  | L
  | R

type command =
  { dir : direction
  ; amount : int
  ; color : string
  }

let next (x, y) dir amount =
  match dir with
  | U -> x, y - amount
  | D -> x, y + amount
  | R -> x + amount, y
  | L -> x - amount, y
;;

let parse_input lines =
  List.map lines ~f:(fun line ->
    let split = String.split line ~on:' ' |> List.to_array in
    let dir =
      match split.(0) with
      | "L" -> L
      | "R" -> R
      | "U" -> U
      | "D" -> D
      | _ -> assert false
    in
    let amount = split.(1) |> Int.of_string in
    let color =
      split.(2)
      |> String.substr_replace_first ~pattern:"(" ~with_:""
      |> String.substr_replace_first ~pattern:")" ~with_:""
      |> String.substr_replace_first ~pattern:"#" ~with_:""
    in
    { dir; amount; color })
;;

let translate plan =
  List.map plan ~f:(fun cmd ->
    match String.to_list cmd.color with
    | [ c1; c2; c3; c4; c5; d1 ] ->
      let amount =
        String.of_list [ '0'; 'x'; c1; c2; c3; c4; c5 ] |> Int.Hex.of_string
      in
      let dir =
        match String.of_list [ d1 ] |> Int.of_string with
        | 0 -> R
        | 1 -> D
        | 2 -> L
        | 3 -> U
        | _ -> assert false
      in
      let color = cmd.color in
      { dir; amount; color }
    | _ -> assert false)
;;

let get_corners plan =
  let rec aux plan coord (corners, perimeter) =
    match plan with
    | [] -> corners, perimeter
    | [ x ] ->
      let n = next coord x.dir x.amount in
      let corners = List.append corners [ n ] in
      aux [] n (corners, perimeter + x.amount)
    | x :: rest ->
      let n = next coord x.dir x.amount in
      let corners = List.append corners [ n ] in
      aux rest n (corners, perimeter + x.amount)
  in
  aux plan (0, 0) ([], 0)
;;

let shoelace coords =
  let all_coords = coords |> List.to_array in
  let n = Array.length all_coords in
  let rec aux coords res idx =
    match coords with
    | [] -> res
    | [ _ ] ->
      let x1, y1 = all_coords.(idx) in
      let x2, y2 = all_coords.((idx + 1) % n) in
      res + ((x1 * y2) - (x2 * y1))
    | coord :: rest ->
      let res = aux [ coord ] res idx in
      aux rest res (idx + 1)
  in
  let x = aux coords 0 0 in
  Int.abs x / 2
;;

let () =
  let plan = Advent.read_lines "./inputs/day18.txt" |> parse_input in
  let corners, perimeter = get_corners plan in
  let _ =
    shoelace corners
    |> Int.( + ) ((perimeter / 2) + 1)
    |> sprintf "Part 1: %d"
    |> print_endline
  in
  let corners, perimeter = translate plan |> get_corners in
  shoelace corners
  |> Int.( + ) ((perimeter / 2) + 1)
  |> sprintf "Part 2: %d"
  |> print_endline
;;
