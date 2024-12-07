open Core

type equation =
  { result : int
  ; nums : int list
  }

let digits num =
  let rec aux num count =
    match num with
    | x when x < 10 -> count + 1
    | _ -> aux (num / 10) count + 1
  in
  aux num 0
;;

let concat left right = (left * Int.pow 10 (digits right)) + right
let operations1 = [ Int.( + ); Int.( * ) ]
let operations2 = concat :: operations1

let is_possible equation operations =
  let rec aux result num1 rest =
    match rest with
    | [] -> num1 = result
    | hd :: rest ->
      List.fold operations ~init:false ~f:(fun acc op ->
        acc || aux result (op num1 hd) rest)
  in
  match equation.nums with
  | hd :: rest -> aux equation.result hd rest
  | _ -> failwith "noob"
;;

let solve equations operations =
  List.map equations ~f:(fun equation ->
    match is_possible equation operations with
    | true -> equation.result
    | false -> 0)
  |> List.fold ~init:0 ~f:Int.( + )
;;

let () =
  let equations =
    Advent.read_lines "inputs/day07.txt"
    |> List.map ~f:(fun line ->
      let not_whitespace str = equal_string str "" |> not in
      let line =
        String.split_on_chars ~on:[ ':'; ' ' ] line
        |> List.filter ~f:not_whitespace
      in
      match line with
      | hd :: rest ->
        { result = Int.of_string hd; nums = List.map rest ~f:Int.of_string }
      | _ -> failwith "bla")
  in
  let _ = solve equations operations1 |> Fmt.pr "Part 1: %d \n" in
  solve equations operations2 |> Fmt.pr "Part 2: %d"
;;
