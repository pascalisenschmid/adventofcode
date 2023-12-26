open Core
open Advent.Parser

let parse_line =
  let* k = take_while (fun c -> Char.(c <> ':')) <* char ':' <* whitespace in
  let* nds =
    sep_by1 (char ' ') (take_while (fun c -> not (Char.is_whitespace c)))
  in
  return (k, nds)
;;

module S = Stdlib.Set.Make (String)
module M = Stdlib.Map.Make (String)

let populate_set start nd edges =
  edges
  |> M.update start (function
    | None -> Some (S.singleton nd)
    | Some set -> Some (S.add nd set))
  |> M.update nd (function
    | None -> Some (S.singleton start)
    | Some set -> Some (S.add start set))
;;

let parse_input lines =
  lines
  |> Stdlib.List.to_seq
  |> Seq.map (fun line ->
    match parse_string ~consume:Prefix parse_line line with
    | Ok (starts, ends) -> starts, ends
    | Error e -> failwith e)
  |> Seq.fold_left
       (fun edges (start, ends) ->
         ends
         |> List.fold_left ~init:edges ~f:(fun edges nd ->
           edges |> populate_set start nd |> populate_set nd start))
       M.empty
;;

let solve edges =
  let rec solve' verts =
    let out v = S.diff (M.find v edges) verts |> S.cardinal in
    let cons =
      S.to_seq verts |> Seq.map (fun v -> v, out v) |> Stdlib.List.of_seq
    in
    if cons |> List.map ~f:snd |> List.fold ~init:0 ~f:Int.( + ) |> Int.( = ) 3
       || S.is_empty verts
    then verts
    else (
      let mx, _ =
        cons
        |> List.fold ~init:("?", -1) ~f:(fun (mv, mc) (v, c) ->
          if c > mc then v, c else mv, mc)
      in
      solve' (S.remove mx verts))
  in
  edges
  |> M.to_seq
  |> Seq.map fst
  |> Seq.map (fun to_remove ->
    solve' (M.to_seq edges |> Seq.map fst |> S.of_seq |> S.remove to_remove))
  |> Seq.drop_while S.is_empty
  |> Seq.uncons
  |> Option.value_exn
  |> fst
;;

let () =
  let edges = Advent.read_lines "./inputs/day25.txt" |> parse_input in
  let n = edges |> solve |> S.cardinal in
  n * (M.cardinal edges - n) |> sprintf "Part 1: %d" |> print_endline
;;

(* heavily inspired by https://github.com/EricKalkman/AoC2023/blob/master/lib/day25.ml thx ._.*)
