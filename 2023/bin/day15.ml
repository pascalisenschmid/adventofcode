open Core
module Map = Stdlib.Map
module Boxes = Map.Make (Int)

let make_boxes =
  List.range 0 256
  |> List.fold ~init:Boxes.empty ~f:(fun boxes i -> Boxes.add i [] boxes)
;;

let calc (c : char) (result : int) =
  let result = Char.to_int c |> Int.( + ) result |> Int.( * ) 17 in
  result mod 256
;;

let hash_algo step =
  let rec aux out chars =
    match chars with
    | [] -> out
    | [ c ] -> calc c out
    | c :: rest -> aux (calc c out) rest
  in
  step |> String.to_list |> aux 0
;;

let split_lenses step =
  let rec aux chars label op =
    match chars with
    | [ '-' ] ->
      let labelstr = label |> String.of_list in
      labelstr, labelstr |> hash_algo, '-', 0
    | [ '='; lens ] ->
      let labelstr = label |> String.of_list in
      labelstr, labelstr |> hash_algo, '=', String.of_char lens |> Int.of_string
    | x :: rest -> aux rest (List.append label [ x ]) op
    | _ -> assert false
  in
  aux (String.to_list step) [] ' '
;;

let remove_label lenses label =
  List.fold lenses ~init:[] ~f:(fun acc (lbl, lens) ->
    match label with
    | x when equal_string x lbl -> acc
    | _ -> List.append acc [ lbl, lens ])
;;

let add_or_change_label lenses (label, lens) =
  match List.find lenses ~f:(fun (lbl, _) -> equal_string lbl label) with
  | Some _ ->
    List.map lenses ~f:(fun (lbl, lns) ->
      match label with
      | x when equal_string x lbl -> label, lens
      | _ -> lbl, lns)
  | None -> List.append lenses [ label, lens ]
;;

let hashmap_algo boxes (label, box, op, lens) =
  let lenses = Boxes.find box boxes in
  let lenses =
    match op with
    | '-' -> remove_label lenses label
    | '=' -> add_or_change_label lenses (label, lens)
    | _ -> assert false
  in
  Boxes.add box lenses boxes
;;

let calc_box_power acc (box, lenses) =
  let power, _ =
    List.fold lenses ~init:(0, 1) ~f:(fun (acc, idx) (_, fl) ->
      let power = (box + 1) * idx * fl in
      acc + power, idx + 1)
  in
  acc + power
;;

let () =
  let lines = Advent.read_lines "./inputs/day15.txt" in
  let _ =
    List.nth_exn lines 0
    |> String.split ~on:','
    |> List.fold ~init:0 ~f:(fun acc line -> acc + hash_algo line)
    |> sprintf "Part 1: %d"
    |> print_endline
  in
  List.nth_exn lines 0
  |> String.split ~on:','
  |> List.map ~f:split_lenses
  |> List.fold ~init:make_boxes ~f:hashmap_algo
  |> Boxes.to_list
  |> List.fold ~init:0 ~f:calc_box_power
  |> sprintf "Part 2: %d"
  |> print_endline
;;
