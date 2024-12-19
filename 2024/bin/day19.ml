open Core

let parse lines =
  match Advent.split_on_empty_line lines with
  | [ first; rest ] ->
    ( List.hd_exn first
      |> String.substr_replace_all ~pattern:" " ~with_:""
      |> String.split ~on:','
    , rest )
  | _ -> failwith "my guy cant copy text"
;;

let combinations towels design =
  let cache = Hashtbl.create (module Int) in
  let rec aux = function
    | i when i >= String.length design -> 1
    | i ->
      Hashtbl.find_or_add cache i ~default:(fun () ->
        towels
        |> List.filter ~f:(fun x ->
          String.is_substring_at design ~pos:i ~substring:x)
        |> List.sum (module Int) ~f:(fun x -> aux (i + String.length x)))
  in
  aux 0
;;

let () =
  let lines = Advent.read_lines "inputs/day19.txt" in
  let towels, designs = parse lines in
  let combinations = combinations towels in
  let possibilities = List.map designs ~f:combinations in
  List.count possibilities ~f:(fun x -> x > 0) |> Fmt.pr "\nPart 1: %d";
  List.fold possibilities ~init:0 ~f:Int.( + ) |> Fmt.pr "\nPart 2: %d";
  ()
;;
