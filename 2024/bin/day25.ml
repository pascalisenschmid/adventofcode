open Core

let read_position row pos =
  match row.(pos) with
  | '#' -> 1
  | _ -> 0
;;

let parse_block rows =
  let init = 0, 0, 0, 0, 0 in
  List.fold rows ~init ~f:(fun (x1, x2, x3, x4, x5) row ->
    let row = String.to_array row in
    let read_position = read_position row in
    ( x1 + read_position 0
    , x2 + read_position 1
    , x3 + read_position 2
    , x4 + read_position 3
    , x5 + read_position 4 ))
;;

let parse blocks =
  List.fold blocks ~init:([], []) ~f:(fun (locks, keys) block ->
    match block with
    | hd :: rest when String.equal hd "#####" -> parse_block rest :: locks, keys
    | block -> locks, (List.drop_last_exn block |> parse_block) :: keys)
;;

let check_combinations (locks, keys) =
  let is_match l k = l + k <= 5 in
  List.fold locks ~init:0 ~f:(fun acc (l1, l2, l3, l4, l5) ->
    List.fold keys ~init:acc ~f:(fun acc (k1, k2, k3, k4, k5) ->
      match
        ( is_match l1 k1
        , is_match l2 k2
        , is_match l3 k3
        , is_match l4 k4
        , is_match l5 k5 )
      with
      | true, true, true, true, true -> acc + 1
      | _ -> acc))
;;

let () =
  Advent.read_lines "inputs/day25.txt"
  |> Advent.split_on_empty_line
  |> parse
  |> check_combinations
  |> Fmt.pr "\nPart 1:%d"
;;
