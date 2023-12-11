open Core

let find_empty_idx lines =
  let _, empty =
    Array.fold lines ~init:(0, []) ~f:(fun (idx, acc) line ->
      let length = String.length line in
      match String.count line ~f:(equal_char '.') with
      | l when l = length -> idx + 1, acc @ [ idx ]
      | _ -> idx + 1, acc)
  in
  empty
;;

let find_stars map =
  let _, stars =
    Array.fold map ~init:(0, []) ~f:(fun (idy, acc) row ->
      let _, stars =
        Array.fold row ~init:(0, []) ~f:(fun (idx, acc) char ->
          match char with
          | '#' -> idx + 1, acc @ [ idx, idy ]
          | _ -> idx + 1, acc)
      in
      idy + 1, acc @ stars)
  in
  stars
;;

let parse_input lines =
  let lines = Array.of_list lines in
  let map, x, _ = Advent.char_matrix lines in
  let vertical_lines =
    List.range 0 x
    |> List.map ~f:(fun x ->
      Array.fold map ~init:[] ~f:(fun acc row -> acc @ [ row.(x) ])
      |> String.of_list)
    |> Array.of_list
  in
  let horizonal_empty = find_empty_idx lines in
  let vertical_empty = find_empty_idx vertical_lines in
  let stars = find_stars map in
  stars, horizonal_empty, vertical_empty
;;

let distance (x1, y1) (x2, y2) h v factor =
  let miny = Int.min y1 y2 in
  let minx = Int.min x1 x2 in
  let maxy = Int.max y1 y2 in
  let maxx = Int.max x1 x2 in
  let exp_h = List.count h ~f:(fun n -> miny < n && n < maxy) in
  let exp_v = List.count v ~f:(fun n -> minx < n && n < maxx) in
  maxy
  - miny
  + maxx
  - minx
  + ((exp_h * factor) - exp_h)
  + ((exp_v * factor) - exp_v)
;;

let solve (stars, h, v) factor templ =
  let rec aux star stars acc =
    match stars with
    | [] -> acc
    | [ s ] -> acc + distance star s h v factor
    | s :: rest -> aux star [ s ] acc |> aux star rest
  in
  let solve_star acc star = acc + aux star stars 0 in
  List.fold stars ~init:0 ~f:solve_star / 2 |> sprintf templ |> print_endline
;;

let () =
  let parsed = Advent.read_lines "./inputs/day11.txt" |> parse_input in
  let _ = print_endline "" in
  let _ = solve parsed 2 "Part 1: %d" in
  solve parsed 1_000_000 "Part 2: %d"
;;
