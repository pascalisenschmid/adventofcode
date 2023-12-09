open Core
module Map = Stdlib.Map
module Nodes = Map.Make (String)

let parse_moves lines =
  match lines with
  | moves :: _ :: rest -> String.to_list moves, rest
  | _ -> assert false
;;

let parse_node line =
  let segments =
    line
    |> String.substr_replace_all ~pattern:" = (" ~with_:" "
    |> String.substr_replace_all ~pattern:", " ~with_:" "
    |> String.substr_replace_all ~pattern:")" ~with_:""
    |> String.split ~on:' '
  in
  match segments with
  | [ name; l; r ] -> name, l, r
  | _ -> assert false
;;

let parse_nodes lines =
  let rec aux lines out =
    match lines with
    | [] -> out
    | [ line ] ->
      let name, l, r = parse_node line in
      Nodes.add name (l, r) out
    | line :: rest -> aux rest (aux [ line ] out)
  in
  aux lines Nodes.empty
;;

let is_ZZZ str = equal_string str "ZZZ"
let ends_in_Z str = equal_char 'Z' (str |> String.to_list |> List.last_exn)

let find allmoves nodes wincondition lr =
  let rec aux moves counter nodes (l, r) =
    match moves with
    | [] -> aux allmoves counter nodes (l, r)
    | [ move ] when equal_char move 'L' ->
      (match l with
       | x when wincondition x -> counter + 1
       | l' ->
         let l, r = Nodes.find l' nodes in
         aux [] (counter + 1) nodes (l, r))
    | [ move ] when equal_char move 'R' ->
      (match r with
       | x when wincondition x -> counter + 1
       | r' ->
         let l, r = Nodes.find r' nodes in
         aux [] (counter + 1) nodes (l, r))
    | move :: rest when equal_char move 'L' ->
      (match l with
       | x when wincondition x -> counter + 1
       | l' ->
         let l, r = Nodes.find l' nodes in
         aux rest (counter + 1) nodes (l, r))
    | move :: rest when equal_char move 'R' ->
      (match r with
       | x when wincondition x -> counter + 1
       | r' ->
         let l, r = Nodes.find r' nodes in
         aux rest (counter + 1) nodes (l, r))
    | _ -> assert false
  in
  aux allmoves 0 nodes lr
;;

let lcm a b =
  let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
  a * b / gcd a b
;;

let solve_part_1 moves nodes =
  nodes
  |> Nodes.find "AAA"
  |> find moves nodes is_ZZZ
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let solve_part_2 moves nodes =
  nodes
  |> Nodes.filter (fun key _ ->
    equal_char 'A' (key |> String.to_list |> List.last_exn))
  |> Nodes.to_list
  |> List.map ~f:(fun (_, (l, r)) -> find moves nodes ends_in_Z (l, r))
  |> List.fold ~init:1 ~f:(fun a b -> lcm a b)
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day8.txt" in
  let moves, lines = parse_moves lines in
  let nodes = parse_nodes lines in
  let _ = solve_part_1 moves nodes in
  solve_part_2 moves nodes
;;
