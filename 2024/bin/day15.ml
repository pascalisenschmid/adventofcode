open Core

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving show, equal, compare]

let next coord dir =
  let open Advent.Matrix in
  match dir with
  | Left -> { x = coord.x; y = coord.y - 1 }
  | Up -> { x = coord.x - 1; y = coord.y }
  | Right -> { x = coord.x; y = coord.y + 1 }
  | Down -> { x = coord.x + 1; y = coord.y }
;;

let parse_moves moves =
  let rec aux acc chars =
    match chars with
    | [] -> acc
    | '^' :: rest -> Up :: aux acc rest
    | '>' :: rest -> Right :: aux acc rest
    | '<' :: rest -> Left :: aux acc rest
    | _ :: rest -> Down :: aux acc rest
  in
  String.to_list moves |> aux []
;;

let transform matrix =
  Array.map matrix ~f:(fun row ->
    Array.fold row ~init:[] ~f:(fun acc c ->
      match c with
      | '#' -> List.append acc [ '#'; '#' ]
      | '.' -> List.append acc [ '.'; '.' ]
      | 'O' -> List.append acc [ '['; ']' ]
      | '@' -> List.append acc [ '@'; '.' ]
      | _ -> failwith "invalid transform")
    |> Array.of_list)
;;

let move char old next matrix =
  let open Advent.Matrix in
  matrix.(next.x).(next.y) <- char;
  matrix.(old.x).(old.y) <- '.';
  matrix
;;

let walk coord moves matrix =
  let rec move_box coord dir moved matrix =
    let next_coord = next coord dir in
    match Advent.Matrix.get_opt matrix next_coord, moved with
    | Some '.', _ -> move 'O' coord next_coord matrix
    | Some '#', _ -> matrix
    | Some 'O', false ->
      move_box next_coord dir false matrix |> move_box coord dir true
    | Some 'O', true -> matrix
    | Some x, _ -> sprintf "invalid box %c" x |> failwith
    | None, _ -> failwith "found nothing box"
  in
  let rec aux coord moves moved matrix =
    match moves with
    | [] -> matrix
    | hd :: rest ->
      let next_coord = next coord hd in
      (match Advent.Matrix.get_opt matrix next_coord, moved with
       | Some '.', _ ->
         aux next_coord rest false (move '@' coord next_coord matrix)
       | Some 'O', true -> aux coord rest false matrix
       | Some 'O', false ->
         move_box next_coord hd false matrix |> aux coord (hd :: rest) true
       | _, _ -> aux coord rest false matrix)
  in
  aux coord moves false matrix
;;

let rec walk2 coord moves matrix =
  let rec can_move_box coord dir matrix =
    let curr_box = Advent.Matrix.get_opt matrix coord in
    let next_coord = next coord dir in
    match curr_box, dir with
    | Some '[', Right | Some ']', Right | Some ']', Left | Some '[', Left ->
      can_move_box next_coord dir matrix
    | Some '[', Up | Some '[', Down ->
      can_move_box next_coord dir matrix
      && can_move_box (next (next coord Right) dir) dir matrix
    | Some ']', Up | Some ']', Down ->
      can_move_box next_coord dir matrix
      && can_move_box (next (next coord Left) dir) dir matrix
    | Some '.', _ -> true
    | _, _ -> false
  in
  let rec move_box coord dir direct_push matrix =
    let curr_box = Advent.Matrix.get_opt matrix coord in
    let next_coord = next coord dir in
    match curr_box, dir with
    | Some '.', _ -> matrix
    | Some c, Right | Some c, Left ->
      move_box next_coord dir true matrix |> move c coord next_coord
    | Some '[', Up | Some '[', Down ->
      let matrix =
        if direct_push
        then move_box (next coord Right) dir false matrix
        else matrix
      in
      matrix |> move_box next_coord dir true |> move '[' coord next_coord
    | Some ']', Up | Some ']', Down ->
      let matrix =
        if direct_push
        then move_box (next coord Left) dir false matrix
        else matrix
      in
      move_box next_coord dir true matrix |> move ']' coord next_coord
    | _, _ -> failwith "noob"
  in
  match moves with
  | [] -> matrix
  | hd :: rest ->
    let next_coord = next coord hd in
    (match Advent.Matrix.get_opt matrix next_coord with
     | Some '.' -> move '@' coord next_coord matrix |> walk2 next_coord rest
     | Some '[' | Some ']' ->
       (match can_move_box next_coord hd matrix with
        | true -> move_box next_coord hd true matrix |> walk2 coord (hd :: rest)
        | false -> walk2 coord rest matrix)
     | _ -> walk2 coord rest matrix)
;;

let sum acc (_, coord) =
  let open Advent.Matrix in
  acc + ((100 * coord.x) + coord.y)
;;

let find_start matrix =
  Advent.Matrix.find_all matrix ~ch:[ '@' ] ~comparer:Char.( = )
  |> List.hd_exn
  |> Tuple2.get2
;;

let part1 matrix moves =
  let matrix =
    Array.of_list matrix |> Advent.Matrix.make ~f:(fun x -> x) |> Tuple3.get1
  in
  let start = find_start matrix in
  walk start moves matrix
  |> Advent.Matrix.find_all ~ch:[ 'O' ] ~comparer:Char.( = )
  |> List.fold ~init:0 ~f:sum
  |> Fmt.pr "\nPart 1: %d";
  ()
;;

let part2 matrix moves =
  let matrix =
    Array.of_list matrix
    |> Advent.Matrix.make ~f:(fun x -> x)
    |> Tuple3.get1
    |> transform
  in
  let start = find_start matrix in
  walk2 start moves matrix
  |> Advent.Matrix.find_all ~ch:[ '[' ] ~comparer:Char.( = )
  |> List.fold ~init:0 ~f:sum
  |> Fmt.pr "\nPart 2: %d";
  ()
;;

let () =
  let input =
    Advent.read_lines "inputs/day15.txt" |> Advent.split_on_empty_line
  in
  let matrix, moves = List.hd_exn input, List.last_exn input in
  let moves = String.concat moves |> parse_moves in
  part1 matrix moves;
  part2 matrix moves
;;
