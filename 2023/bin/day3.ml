open Core

let get_symbols_pos line symbols =
  String.to_list line
  |> List.filter_mapi ~f:(fun idx c ->
    let exists = List.mem ~equal:phys_equal symbols c in
    match exists with
    | true -> Some idx
    | _ -> None)
;;

let get_number list first last =
  match first with
  | x when x > last -> 0
  | _ ->
    List.sub ~pos:first ~len:(last - first + 1) list
    |> String.of_list
    |> Int.of_string
;;

let find_from_back line symbol_pos =
  let rec aux charlist cursor =
    match List.nth charlist cursor with
    | Some c when Char.is_digit c -> aux charlist (cursor - 1)
    | _ -> get_number charlist (cursor + 1) (symbol_pos - 1)
  in
  match line with
  | Some l ->
    let chars = String.to_list l in
    (match List.nth chars symbol_pos with
     | Some c when Char.is_digit c -> 0
     | _ -> aux chars (symbol_pos - 1))
  | _ -> 0
;;

let find_from_front line symbol_pos =
  let rec aux charlist cursor =
    match List.nth charlist cursor with
    | Some c when Char.is_digit c -> aux charlist (cursor + 1)
    | _ -> get_number charlist (symbol_pos + 1) (cursor - 1)
  in
  match line with
  | Some l ->
    let chars = String.to_list l in
    (match List.nth chars symbol_pos with
     | Some c when Char.is_digit c -> 0
     | _ -> aux chars (symbol_pos + 1))
  | _ -> 0
;;

let find_from_within line symbol_pos =
  let rec aux charlist cursor =
    match List.nth charlist cursor with
    | Some c when Char.is_digit c -> aux charlist (cursor - 1)
    | _ -> find_from_front line cursor
  in
  match line with
  | Some l ->
    let chars = String.to_list l in
    (match List.nth chars symbol_pos with
     | Some c when Char.is_digit c -> aux (String.to_list l) symbol_pos
     | _ -> 0)
  | _ -> 0
;;

let operations prev curr next pos =
  [ find_from_back prev pos
  ; find_from_back curr pos
  ; find_from_back next pos
  ; find_from_front prev pos
  ; find_from_front curr pos
  ; find_from_front next pos
  ; find_from_within prev pos
  ; find_from_within next pos
  ]
;;

let walk lines aux idx line =
  let prev = List.nth lines (idx - 1) in
  let next = List.nth lines (idx + 1) in
  aux prev line next
;;

let solve_part_one lines =
  let aux prev curr next =
    get_symbols_pos curr [ '*'; '%'; '/'; '-'; '#'; '@'; '&'; '+'; '='; '$' ]
    |> List.fold ~init:0 ~f:(fun acc pos ->
      List.map (operations prev (Some curr) next pos) ~f:(fun operation ->
        operation)
      |> List.fold ~init:0 ~f:( + )
      |> Int.( + ) acc)
  in
  List.mapi lines ~f:(walk lines aux)
  |> List.fold ~init:0 ~f:( + )
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let solve_part_two lines =
  let aux prev curr next =
    get_symbols_pos curr [ '*' ]
    |> List.fold ~init:0 ~f:(fun acc pos ->
      let filter nums =
        match List.length nums with
        | 2 -> List.fold nums ~init:1 ~f:( * ) + acc
        | _ -> acc
      in
      List.map (operations prev (Some curr) next pos) ~f:(fun operation ->
        operation)
      |> List.filter ~f:(fun n -> n > 0)
      |> filter)
  in
  List.mapi lines ~f:(walk lines aux)
  |> List.fold ~init:0 ~f:( + )
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day3.txt" in
  let _ = solve_part_one lines in
  solve_part_two lines
;;
