open Core

type change_type =
  | None
  | Increasing
  | Decreasing

let parse_line line =
  String.split_on_chars ~on:[ ' ' ] line |> List.map ~f:Int.of_string
;;

let calculate_diffs numbers =
  let rec aux nums out =
    match nums with
    | [] -> out
    | [ _ ] -> out
    | [ l; r ] -> List.append out [ r - l ]
    | l :: r :: rest ->
      List.append out [ r - l ] |> aux (List.append [ r ] rest)
  in
  aux numbers []
;;

let expand_lists numbers =
  List.length numbers
  |> Advent.range_seq 0
  |> Seq.map (fun idx -> Advent.remove_list_item numbers idx)
  |> Stdlib.List.of_seq
  |> List.append [ numbers ]
;;

let check_diffs (success, change_type) num =
  match success, change_type, num with
  | true, None, x when x >= 1 && x <= 3 -> true, Increasing
  | true, None, x when x >= -3 && x <= -1 -> true, Decreasing
  | true, Increasing, x when x >= 1 && x <= 3 -> true, Increasing
  | true, Decreasing, x when x <= -1 && x >= -3 -> true, Decreasing
  | _ -> false, change_type
;;

let analyze_part1 transformed =
  List.fold transformed ~init:(true, None) ~f:check_diffs |> Tuple.T2.get1
;;

let analyze_part2 transformed =
  List.map transformed ~f:(fun nums -> calculate_diffs nums |> analyze_part1)
  |> List.exists ~f:(fun x -> equal_bool x true)
;;

let solve input solver =
  List.fold input ~init:0 ~f:(fun acc line ->
    match solver line with
    | true -> acc + 1
    | false -> acc)
;;

let solver1 line = line |> parse_line |> calculate_diffs |> analyze_part1
let solver2 line = line |> parse_line |> expand_lists |> analyze_part2

let () =
  let input = Advent.read_lines "inputs/day02.txt" in
  let _ = solve input solver1 |> sprintf "Part 1: %d" |> print_endline in
  solve input solver2 |> sprintf "Part 2: %d" |> print_endline
;;
