open Core

let parse_lines lines =
  let rec aux lines out =
    match lines with
    | [] -> out
    | [ line ] ->
      let l = line |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      out @ [ l ]
    | line :: rest -> aux rest (aux [ line ] out)
  in
  aux lines []
;;

let rec solve nums =
  let zeros = List.filter nums ~f:(fun x -> x = 0) |> List.length in
  let r =
    match List.length nums with
    | x when x = zeros -> 0
    | _ ->
      List.folding_map nums ~init:None ~f:(fun prev num ->
        match prev with
        | None -> Some num, None
        | Some x -> Some num, Some (num - x))
      |> List.filter_map ~f:(fun x -> x)
      |> solve
  in
  List.hd_exn nums + r
;;

let rec solve2 nums =
  let zeros = List.filter nums ~f:(fun x -> x = 0) |> List.length in
  match List.length nums with
  | x when x = zeros -> 0
  | _ ->
    let r =
      List.folding_map nums ~init:None ~f:(fun prev num ->
        match prev with
        | None -> Some num, None
        | Some x -> Some num, Some (num - x))
      |> List.filter_map ~f:(fun x -> x)
      |> solve2
    in
    List.hd_exn nums - r
;;

let () =
  let lines = Advent.read_lines "./inputs/day9.txt" |> parse_lines in
  let _ =
    lines
    |> List.fold ~init:0 ~f:(fun acc nums -> acc + solve nums)
    |> sprintf "Part 1: %d"
    |> print_endline
  in
  lines
  |> List.fold ~init:0 ~f:(fun acc nums -> acc + solve2 nums)
  |> sprintf "Part 2: %d"
  |> print_endline
;;
