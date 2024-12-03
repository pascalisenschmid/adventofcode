open Core
open Advent

let part1 input =
  let regex = Re.Perl.compile_pat "mul\\((\\d{1,3}),(\\d{1,3})\\)" in
  input
  |> Re.all regex
  |> List.fold ~init:0 ~f:(fun acc group ->
    match Re.Group.get_opt group 1, Re.Group.get_opt group 2 with
    | Some left, Some right -> acc + (Int.of_string left * Int.of_string right)
    | _ -> failwith "damn")
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let part2 input =
  let regex =
    Re.Perl.compile_pat
      "mul\\((\\d{1,3}),(\\d{1,3})\\)|(do\\(\\))|(don't\\(\\))"
  in
  input
  |> Re.all regex
  |> List.fold ~init:(0, true) ~f:(fun (total, should_add) group ->
    let get_match = Re.Group.get_opt group in
    let try_add left right =
      match should_add with
      | true -> total + (Int.of_string left * Int.of_string right), true
      | false -> total, false
    in
    match get_match 1, get_match 2, get_match 3, get_match 4 with
    | Some left, Some right, None, None -> try_add left right
    | None, None, Some _, None -> total, true
    | None, None, None, Some _ -> total, false
    | _ -> failwith "oops")
  ||> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let input =
    Advent.read_lines "inputs/day03.txt"
    |> List.fold ~init:"" ~f:(fun acc line -> String.append acc line)
  in
  let _ = part1 input in
  part2 input
;;
