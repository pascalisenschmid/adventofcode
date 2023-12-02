open Core

let split_color input =
  let splt = String.strip input |> String.split_on_chars ~on:[ ' ' ] in
  match splt with
  | amount :: color :: _ -> color, Int.of_string amount
  | _ -> "", 0
;;

let get_colors line =
  let splits = String.split_on_chars ~on:[ ':'; ','; ';' ] line in
  match splits with
  | [] -> []
  | _ :: colors -> List.map colors ~f:split_color
;;

let checkamount (color, amount) =
  match color with
  | "blue" -> Int.( <= ) amount 14
  | "red" -> Int.( <= ) amount 12
  | "green" -> Int.( <= ) amount 13
  | _ -> false
;;

let solve_part_one input =
  List.foldi input ~init:0 ~f:(fun idx acc line ->
    let colors = get_colors line in
    let falses =
      List.map colors ~f:(fun color -> checkamount color)
      |> List.filter ~f:(fun b -> equal_bool b false)
      |> List.length
    in
    acc
    +
    match falses with
    | 0 -> idx + 1
    | _ -> 0)
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let solve_part_two input =
  List.fold input ~init:0 ~f:(fun acc line ->
    let colors = get_colors line in
    let check_col (c, a) (blue, red, green) =
      match c with
      | "blue" -> Int.max a blue, red, green
      | "red" -> blue, Int.max a red, green
      | "green" -> blue, red, Int.max a green
      | _ -> blue, red, green
    in
    let rec aux colors maxes =
      match colors with
      | [] -> maxes
      | [ color ] -> check_col color maxes
      | color :: rest -> aux rest (check_col color maxes)
    in
    let blue, red, green = aux colors (0, 0, 0) in
    acc + (blue * red * green))
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day2.txt" in
  let _ = solve_part_one lines in
  solve_part_two lines
;;
