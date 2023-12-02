open Core

let convert str =
  String.substr_replace_all str ~pattern:"one" ~with_:"one1one"
  |> String.substr_replace_all ~pattern:"two" ~with_:"two2two"
  |> String.substr_replace_all ~pattern:"three" ~with_:"three3three"
  |> String.substr_replace_all ~pattern:"four" ~with_:"four4four"
  |> String.substr_replace_all ~pattern:"five" ~with_:"five5five"
  |> String.substr_replace_all ~pattern:"six" ~with_:"six6six"
  |> String.substr_replace_all ~pattern:"seven" ~with_:"seven7seven"
  |> String.substr_replace_all ~pattern:"eight" ~with_:"eight8eight"
  |> String.substr_replace_all ~pattern:"nine" ~with_:"nine9nine"
;;

let part1 line =
  let numbers = String.to_list line |> List.filter ~f:Char.is_digit in
  sprintf "%c%c" (List.hd_exn numbers) (List.last_exn numbers) |> Int.of_string
;;

let part2 line =
  let numbers =
    convert line |> String.to_list |> List.filter ~f:Char.is_digit
  in
  sprintf "%c%c" (List.hd_exn numbers) (List.last_exn numbers) |> Int.of_string
;;

let solve lines proc title =
  List.map lines ~f:proc
  |> List.fold ~init:0 ~f:( + )
  |> sprintf title
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day1.txt" in
  let _ = solve lines part1 "Part 1: %d" in
  solve lines part2 "Part 2: %d"
;;
