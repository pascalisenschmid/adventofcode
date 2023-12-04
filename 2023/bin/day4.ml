open Core
module Map = Stdlib.Map

module Scratchcard = struct
  type t =
    { amount : int
    ; winners : int
    }
end

let extract_nums str =
  String.strip str
  |> String.split ~on:' '
  |> List.filter ~f:(fun s ->
    match s with
    | "" -> false
    | _ -> true)
  |> List.map ~f:Int.of_string
;;

let get_winners_amount winners_str mynums_str =
  let winning = extract_nums winners_str in
  let my_nums = extract_nums mynums_str in
  List.fold winning ~init:0 ~f:(fun acc winner ->
    match List.mem ~equal:phys_equal my_nums winner with
    | true -> acc + 1
    | false -> acc)
;;

let parse_card line copyamount =
  let parts = String.split_on_chars line ~on:[ ':'; '|' ] in
  let winners =
    get_winners_amount (List.nth_exn parts 1) (List.nth_exn parts 2)
  in
  Scratchcard.{ amount = 1 + copyamount; winners }
;;

module Copies = Map.Make (Int)

let parse_card_2 idx copies line =
  let rec aux idx winners card_amount copies =
    match winners with
    | 0 -> copies
    | _ ->
      (match Copies.find_opt idx copies with
       | Some x ->
         let copies = Copies.add idx (x + card_amount) copies in
         aux (idx + 1) (winners - 1) card_amount copies
       | None ->
         let copies = Copies.add idx card_amount copies in
         aux (idx + 1) (winners - 1) card_amount copies)
  in
  let idx = idx + 1 in
  let copyamount =
    match Copies.find_opt idx copies with
    | Some x -> x
    | None -> 0
  in
  let card = parse_card line copyamount in
  let copies = aux (idx + 1) card.winners card.amount copies in
  copies, card.amount
;;

let solve_part_1 lines =
  List.map lines ~f:(fun line ->
    let card = parse_card line 0 in
    match card.winners with
    | 0 -> 0
    | n -> Int.pow 2 (n - 1))
  |> List.fold ~init:0 ~f:Int.( + )
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let solve_part_2 lines =
  let _, card_amounts =
    List.fold_mapi lines ~init:Copies.empty ~f:parse_card_2
  in
  List.fold card_amounts ~init:0 ~f:Int.( + )
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day4.txt" in
  let _ = solve_part_1 lines in
  solve_part_2 lines
;;
