open Core

type direction =
  | Unknown
  | Left
  | UpLeft
  | Up
  | UpRight
  | Right
  | DownRight
  | Down
  | DownLeft
[@@deriving show]

let dirs = [ Left; UpLeft; Up; UpRight; Right; DownRight; Down; DownLeft ]

let next (x, y) = function
  | Left -> x, y - 1
  | UpLeft -> x - 1, y - 1
  | Up -> x - 1, y
  | UpRight -> x - 1, y + 1
  | Right -> x, y + 1
  | DownRight -> x + 1, y + 1
  | Down -> x + 1, y
  | DownLeft -> x + 1, y - 1
  | Unknown -> failwith "you suck"
;;

let get_char_opt matrix (x, y) =
  try Some matrix.(x).(y) with
  | Failure _ | Invalid_argument _ -> None
;;

let find_char char matrix =
  Array.foldi
    ~init:[]
    ~f:(fun idx acc row ->
      Array.foldi row ~init:acc ~f:(fun idy acc c ->
        match c with
        | c when Char.( = ) c char -> List.append acc [ (idx, idy), Unknown ]
        | _ -> acc))
    matrix
;;

let find_next char matrix coords =
  let aux coord acc dir =
    let new_coord = next coord dir in
    match get_char_opt matrix new_coord with
    | Some c when Char.( = ) c char -> List.append acc [ new_coord, dir ]
    | _ -> acc
  in
  List.fold coords ~init:[] ~f:(fun acc (coord, dir) ->
    let aux = aux coord in
    match dir with
    | Unknown -> List.fold dirs ~init:acc ~f:aux
    | dir -> aux acc dir)
;;

let find_cross matrix coords =
  List.fold coords ~init:[] ~f:(fun acc (coord, _) ->
    let next = next coord in
    let gco = get_char_opt matrix in
    let ul = next UpLeft in
    let ur = next UpRight in
    let dl = next DownLeft in
    let dr = next DownRight in
    match gco ul, gco dr, gco ur, gco dl with
    | Some 'M', Some 'S', Some 'M', Some 'S' -> List.append acc [ coord ]
    | Some 'S', Some 'M', Some 'M', Some 'S' -> List.append acc [ coord ]
    | Some 'M', Some 'S', Some 'S', Some 'M' -> List.append acc [ coord ]
    | Some 'S', Some 'M', Some 'S', Some 'M' -> List.append acc [ coord ]
    | _ -> acc)
;;

let () =
  let matrix, _, _ =
    Advent.read_lines "inputs/day04.txt"
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun x -> x)
  in
  find_char 'X' matrix
  |> find_next 'M' matrix
  |> find_next 'A' matrix
  |> find_next 'S' matrix
  |> List.length
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let () =
  let matrix, _, _ =
    Advent.read_lines "inputs/day04.txt"
    |> Array.of_list
    |> Advent.Matrix.make ~f:(fun x -> x)
  in
  find_char 'A' matrix
  |> find_cross matrix
  |> List.length
  |> sprintf "Part 2: %d"
  |> print_endline
;;
