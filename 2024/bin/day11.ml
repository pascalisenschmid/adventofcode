open Core
module Map = Stdlib.Map.Make (Int)

let insert_or_increment map n count =
  Map.update
    n
    (function
      | Some x -> Some (x + count)
      | None -> Some count)
    map
;;

let digits num =
  if num = 0
  then 1
  else num |> Float.of_int |> Float.log10 |> Int.of_float |> Int.( + ) 1
;;

let split num digits =
  let x = Int.pow 10 (digits / 2) in
  let right = num % x in
  let left = num / x in
  left, right
;;

let transform (map : int Map.t) =
  Map.fold
    (fun key count acc ->
       let digits = digits key in
       match key with
       | 0 -> insert_or_increment acc 1 count
       | n when digits % 2 = 0 ->
         let left, right = split n digits in
         let acc = insert_or_increment acc left count in
         insert_or_increment acc right count
       | n -> insert_or_increment acc (n * 2024) count)
    map
    Map.empty
;;

let transforms amount map =
  Advent.range_seq 1 amount |> Seq.fold_left (fun acc _ -> transform acc) map
;;

let () =
  let map =
    Advent.read_lines "inputs/day11.txt"
    |> List.hd_exn
    |> String.split ~on:' '
    |> List.map ~f:Int.of_string
    |> List.fold ~init:Map.empty ~f:(fun acc n -> insert_or_increment acc n 1)
  in
  let part1 = transforms 25 map in
  let part2 = transforms 75 map in
  Map.fold (fun _ v acc -> acc + v) part1 0 |> Fmt.pr "\nPart 1: %d";
  Map.fold (fun _ v acc -> acc + v) part2 0 |> Fmt.pr "\nPart 2: %d"
;;
