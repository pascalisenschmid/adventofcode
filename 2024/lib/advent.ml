open Core

let ( ||> ) (l, _) f = f l
let read_lines file = In_channel.read_lines file
let read_all file = In_channel.read_all file
let read_lines_seq file = read_lines file |> Stdlib.List.to_seq

let split_on_empty_line lines =
  let rec aux lines acc block_acc =
    match lines with
    | [] -> List.append acc [ block_acc ]
    | [ line ] -> aux [] acc (List.append block_acc [ line ])
    | hd :: tl when String.is_empty hd ->
      aux tl (List.append acc [ block_acc ]) []
    | hd :: tl -> aux tl acc (List.append block_acc [ hd ])
  in
  aux lines [] []
;;

let remove_list_item list idx =
  let rec aux list cursor acc =
    match list with
    | [] -> acc
    | _ :: tl when idx = cursor -> aux tl (cursor + 1) acc
    | hd :: tl -> aux tl (cursor + 1) (hd :: acc)
  in
  aux list 0 []
;;

let range_seq start stop =
  let next i = if i > stop then None else Some (i, i + 1) in
  Seq.unfold next start
;;

let char_matrix lines =
  let x = Array.length lines in
  let y = lines.(0) |> String.to_array |> Array.length in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun y row ->
      let line = lines.(y) |> String.to_array in
      Array.mapi row ~f:(fun x _ -> line.(x)))
  in
  map, x, y
;;

module Parser = struct
  include Angstrom

  let digit =
    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    in
    let* prefix = peek_char in
    match prefix with
    | Some '-' ->
      let* _ = char '-' in
      take_while1 is_digit >>| Int.of_string >>| Int.neg
    | _ -> take_while1 is_digit >>| Int.of_string
  ;;

  let fdigit = digit >>| Int.to_float
  let whitespace = take_while Char.is_whitespace
  let skip_whitespace = skip_many
  let newline = string "\n"
end
