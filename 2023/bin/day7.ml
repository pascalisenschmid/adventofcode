open Core

let get_value c =
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | c -> sprintf "%c" c |> Int.of_string
;;

let get_value2 c =
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 1
  | 'T' -> 10
  | c -> sprintf "%c" c |> Int.of_string
;;

let type_to_int groups =
  match List.length groups with
  | 1 -> 7
  | 2 ->
    (match groups with
     | x :: _ when x = 4 -> 6
     | _ -> 5)
  | 3 ->
    (match groups with
     | x :: _ when x = 3 -> 4
     | _ -> 3)
  | 4 -> 2
  | _ -> 1
;;

let hand_comparer
  resolver
  ((h1 : string), (_ : int), (t1 : int))
  ((h2 : string), (_ : int), (t2 : int))
  =
  let rec aux (h1', h2') =
    match h1', h2' with
    | [ c1 ], [ c2 ] when resolver c1 > resolver c2 -> 1
    | [ c1 ], [ c2 ] when resolver c1 < resolver c2 -> -1
    | c1 :: _, c2 :: _ when resolver c1 > resolver c2 -> 1
    | c1 :: _, c2 :: _ when resolver c1 < resolver c2 -> -1
    | _ :: r1, _ :: r2 -> aux (r1, r2)
    | _ -> 0
  in
  match t1 with
  | t1' when t1' > t2 -> 1
  | t1' when t1' < t2 -> -1
  | _ -> aux (String.to_list h1, String.to_list h2)
;;

let group1 hand =
  hand
  |> String.to_list
  |> List.sort_and_group ~compare:compare_char
  |> List.map ~f:List.length
  |> List.sort ~compare:Advent.compare_desc
  |> type_to_int
;;

let group2 hand =
  let insert_jokers jokers grouped_ints =
    match jokers with
    | 0 -> grouped_ints
    | x ->
      (match grouped_ints with
       | [ a ] -> [ a + x ]
       | a :: rest -> [ a + x ] @ rest
       | _ -> assert false)
  in
  let replace_jokers hand =
    let hand = String.substr_replace_all hand ~pattern:"J" ~with_:"" in
    match String.length hand with
    | 0 -> 0, "JJJJJ"
    | x -> 5 - x, hand
  in
  let jokers, hand = replace_jokers hand in
  String.to_list hand
  |> List.sort_and_group ~compare:compare_char
  |> List.map ~f:List.length
  |> List.sort ~compare:Advent.compare_desc
  |> insert_jokers jokers
  |> type_to_int
;;

let parse_hands lines group =
  List.map lines ~f:(fun line ->
    let s = String.split_on_chars line ~on:[ ' ' ] in
    List.nth_exn s 0, List.nth_exn s 1)
  |> List.map ~f:(fun (hand, bid) -> hand, Int.of_string bid, group hand)
;;

let solve_part_1 lines =
  let comparer = hand_comparer get_value in
  parse_hands lines group1
  |> List.sort ~compare:comparer
  |> List.mapi ~f:(fun idx (_, bet, _) -> (idx + 1) * bet)
  |> List.fold ~init:0 ~f:( + )
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let solve_part_2 lines =
  let comparer = hand_comparer get_value2 in
  parse_hands lines group2
  |> List.sort ~compare:comparer
  |> List.mapi ~f:(fun idx (_, bet, _) -> (idx + 1) * bet)
  |> List.fold ~init:0 ~f:( + )
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day7.txt" in
  let _ = solve_part_1 lines in
  solve_part_2 lines
;;
