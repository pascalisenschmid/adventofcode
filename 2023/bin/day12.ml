open Core
module Map = Stdlib.Map

module Cache = Map.Make (struct
    type t = char list * int list [@@deriving compare]
  end)

let split_lines lines =
  List.map lines ~f:(fun line ->
    let parts = String.split line ~on:' ' in
    ( List.nth_exn parts 0 |> String.to_list
    , List.nth_exn parts 1 |> String.split ~on:',' |> List.map ~f:Int.of_string
    ))
;;

let unfold_lines lines =
  List.map lines ~f:(fun (springs, groups) ->
    let rec aux copies springs groups springs_out groups_out =
      match copies with
      | 0 -> springs_out, groups_out
      | _ ->
        let springs_out = List.append springs_out [ '?' ] in
        let springs_out = List.append springs_out springs in
        let groups_out = List.append groups_out groups in
        aux (copies - 1) springs groups springs_out groups_out
    in
    aux 4 springs groups springs groups)
;;

let rec cut springs amount =
  match springs, amount with
  | [], _ -> []
  | s, 0 -> s
  | _ :: r, a -> cut r (a - 1)
;;

let rec solve springs groups cache out =
  match springs, groups with
  | [], [] -> cache, out + 1
  | spr, [] ->
    (match List.find spr ~f:(fun c -> equal_char c '#') with
     | Some _ -> cache, out
     | None -> cache, out + 1)
  | springs, groups ->
    let cachehit = Cache.find_opt (springs, groups) cache in
    (match cachehit with
     | Some x -> cache, x
     | None ->
       let cache, result =
         match springs with
         | '.' :: rest | '?' :: rest -> solve rest groups cache out
         | _ -> cache, out
       in
       let cache, result2 =
         match springs, groups with
         | s, num :: _ when num > List.length s -> cache, 0
         | s, num :: _
           when List.take s num
                |> List.find ~f:(fun c -> equal_char c '.')
                |> Option.is_some -> cache, 0
         | s, num :: rest
           when List.length s = num || not (equal_char (List.nth_exn s num) '#')
           -> solve (cut s (num + 1)) rest cache out
         | _ -> cache, 0
       in
       let result = result + result2 in
       let cache = Cache.add (springs, groups) result cache in
       cache, result)
;;

let do_it lines templ =
  let _, result =
    List.fold
      lines
      ~init:(Cache.empty, 0)
      ~f:(fun (cache, acc) (springs, groups) ->
        let cache, matches = solve springs groups cache 0 in
        cache, acc + matches)
  in
  result |> sprintf templ |> print_endline
;;

let _ =
  let lines = Advent.read_lines "./inputs/day12.txt" in
  let lines = split_lines lines in
  let _ = do_it lines "Part 1: %d" in
  let lines = unfold_lines lines in
  do_it lines "Part 2: %d"
;;
