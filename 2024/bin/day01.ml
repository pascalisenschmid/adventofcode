open Core

let sort_int_list list = List.sort list ~compare:Int.compare

let split_and_parse line =
  let nums = String.split_on_chars ~on:[ ' ' ] line in
  List.hd_exn nums |> Int.of_string, List.last_exn nums |> Int.of_string
;;

let split_to_lists input =
  let l1, l2 = input |> List.map ~f:split_and_parse |> List.unzip in
  sort_int_list l1, sort_int_list l2
;;

let count_occurences item list =
  List.count ~f:(fun item2 -> equal_int item item2) list
;;

let diff a b = a - b |> abs

let part1 l1 l2 =
  List.zip_exn l1 l2
  |> List.fold ~init:0 ~f:(fun acc (l, r) -> diff l r |> ( + ) acc)
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let part2 l1 l2 =
  List.fold ~init:0 ~f:(fun acc num -> acc + (num * count_occurences num l2)) l1
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let l1, l2 = Advent.read_lines "inputs/day01.txt" |> split_to_lists in
  let _ = part1 l1 l2 in
  part2 l1 l2
;;
