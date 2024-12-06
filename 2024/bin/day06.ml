open Core

type coord = int * int [@@deriving equal, compare]

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving show, equal, compare]

module Set = Stdlib.Set.Make (struct
    type t = coord [@@deriving compare]
  end)

module Set2 = Stdlib.Set.Make (struct
    type t = coord * direction [@@deriving compare]
  end)

let next (x, y) = function
  | Left -> x, y - 1
  | Up -> x - 1, y
  | Right -> x, y + 1
  | Down -> x + 1, y
;;

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up
;;

let find_char char matrix =
  Array.foldi
    ~init:(false, (0, 0))
    ~f:(fun idx (found, coords) row ->
      match found with
      | true -> found, coords
      | false ->
        Array.foldi row ~init:(found, coords) ~f:(fun idy (found, coords) c ->
          match found with
          | true -> found, coords
          | false ->
            (match c with
             | c when Char.( = ) c char -> true, (idx, idy)
             | _ -> found, coords)))
    matrix
  |> Tuple2.get2
;;

let get_char_opt matrix (x, y) =
  try Some matrix.(x).(y) with
  | Failure _ | Invalid_argument _ -> None
;;

let rec walk matrix set curr_pos curr_dir =
  let next_pos = next curr_pos curr_dir in
  let x, y = next_pos in
  match get_char_opt matrix next_pos with
  | None -> set
  | Some '#' -> walk matrix set curr_pos (turn_right curr_dir)
  | Some '.' | Some '^' ->
    let set = Set.add next_pos set in
    walk matrix set next_pos curr_dir
  | Some c -> sprintf "Found this: %c at %d %d" c x y |> failwith
;;

let walk2 matrix curr_pos curr_dir p1set =
  let rec detect_cycle matrix set curr_pos curr_dir obstacle_pos =
    let next_pos = next curr_pos curr_dir in
    match get_char_opt matrix next_pos with
    | None -> 0
    | Some '#' ->
      detect_cycle matrix set curr_pos (turn_right curr_dir) obstacle_pos
    | Some '.' when equal_coord next_pos obstacle_pos ->
      detect_cycle matrix set curr_pos (turn_right curr_dir) obstacle_pos
    | Some '.' | Some '^' ->
      let entry = next_pos, curr_dir in
      (match Set2.mem entry set with
       | true -> 1
       | false ->
         let set = Set2.add entry set in
         detect_cycle matrix set next_pos curr_dir obstacle_pos)
    | Some c -> sprintf "Found this: %c" c |> failwith
  in
  Set.fold
    (fun obstacle acc ->
       acc + detect_cycle matrix Set2.empty curr_pos curr_dir obstacle)
    p1set
    0
;;

let () =
  let matrix, _, _ =
    Advent.read_lines "inputs/day06.txt" |> Array.of_list |> Advent.char_matrix
  in
  let start_pos = find_char '^' matrix in
  let start_dir = Up in
  let set = Set.empty |> Set.add start_pos in
  let set = walk matrix set start_pos start_dir in
  let _ = Set.cardinal set |> sprintf "Part 1: %d" |> print_endline in
  let _ =
    walk2 matrix start_pos start_dir set
    |> sprintf "Part 2: %d"
    |> print_endline
  in
  ()
;;
