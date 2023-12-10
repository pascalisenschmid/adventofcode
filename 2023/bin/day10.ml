open Core

type direction =
  | Up
  | Down
  | Left
  | Right
  | DeadEnd
  | Loop

type pipe =
  | NS
  | EW
  | NW
  | NE
  | SE
  | SW
  | Empty

let create_map lines =
  let lines = List.to_array lines in
  let y = lines.(0) |> String.to_list |> List.length in
  let x = lines |> Array.length in
  let start = ref (0, 0) in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun x row ->
      let line = lines.(x) |> String.to_array in
      Array.mapi row ~f:(fun y _ ->
        let c = line.(y) in
        match c with
        | c when equal_char c 'S' ->
          start := x, y;
          c
        | _ -> c))
  in
  map, !start
;;

let get_next_dir map (x, y) dir =
  try
    let char = map.(x).(y) in
    match dir, char with
    | Up, '|' -> Up, NS
    | Up, '7' -> Left, SW
    | Up, 'F' -> Right, SE
    | Down, '|' -> Down, NS
    | Down, 'J' -> Left, NW
    | Down, 'L' -> Right, NE
    | Left, '-' -> Left, EW
    | Left, 'F' -> Down, SE
    | Left, 'L' -> Up, NE
    | Right, '-' -> Right, EW
    | Right, 'J' -> Up, NW
    | Right, '7' -> Down, SW
    | _, 'S' -> Loop, Empty
    | _ -> DeadEnd, Empty
  with
  | Failure _ -> DeadEnd, Empty
  | Invalid_argument _ -> DeadEnd, Empty
;;

let get_next_coords (x, y) dir =
  match dir with
  | Up -> x - 1, y
  | Down -> x + 1, y
  | Right -> x, y + 1
  | Left -> x, y - 1
  | _ -> assert false
;;

let add_corner pipe (x, y) coords =
  match pipe with
  | NE | NW | SE | SW -> List.append coords [ x, y ]
  | _ -> coords
;;

let insert_start first_dir last_dir coords (x, y) =
  match first_dir, last_dir with
  | _, DeadEnd -> coords
  | Up, Right | Left, Down -> List.append [ x, y ] coords
  | Down, Right | Left, Up -> List.append [ x, y ] coords
  | Down, Left | Right, Up -> List.append [ x, y ] coords
  | Right, Down | Up, Left -> List.append [ x, y ] coords
  | _ -> assert false
;;

let rec walk_dir map counter dir (x, y) coords =
  let next, pipe = get_next_dir map (x, y) dir in
  match next with
  | Loop -> counter, coords, dir
  | DeadEnd -> counter, coords, DeadEnd
  | Up ->
    let coords = add_corner pipe (x, y) coords in
    walk_dir map (counter + 1) Up (get_next_coords (x, y) Up) coords
  | Down ->
    let coords = add_corner pipe (x, y) coords in
    walk_dir map (counter + 1) Down (get_next_coords (x, y) Down) coords
  | Right ->
    let coords = add_corner pipe (x, y) coords in
    walk_dir map (counter + 1) Right (get_next_coords (x, y) Right) coords
  | Left ->
    let coords = add_corner pipe (x, y) coords in
    walk_dir map (counter + 1) Left (get_next_coords (x, y) Left) coords
;;

let walk map start =
  let rec aux (directions : direction list) start =
    match directions with
    | [] -> 0, []
    | [ direction ] ->
      let r, coords, dir =
        walk_dir map 0 direction (get_next_coords start direction) []
      in
      (match dir with
       | DeadEnd -> aux [] start
       | _ ->
         let coords = insert_start direction dir coords start in
         r, coords)
    | direction :: rest ->
      let r, coords, dir =
        walk_dir map 0 direction (get_next_coords start direction) []
      in
      (match dir with
       | DeadEnd -> aux rest start
       | _ ->
         let coords = insert_start direction dir coords start in
         r, coords)
  in
  let r, coords = aux [ Up; Down; Left; Right ] start in
  r, coords
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
  Int.shift_right (Int.abs x) 1
;;

let part1 result = ((result - 1) / 2) + 1

let () =
  let map, start = Advent.read_lines "./inputs/day10.txt" |> create_map in
  let result, coords = walk map start in
  let _ = result |> part1 |> sprintf "Part 1: %d" |> print_endline in
  shoelace coords - part1 result + 1 |> sprintf "Part 2: %d" |> print_endline
;;
