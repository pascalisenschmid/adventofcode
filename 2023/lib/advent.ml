open Core

let read_lines file = In_channel.read_lines file
let read_all file = In_channel.read_all file

let string_is_int str =
  try
    int_of_string str |> ignore;
    true
  with
  | Failure _ -> false
;;

let print_string_list list = List.iter list ~f:print_endline
let compare_desc a b = compare a b * -1

let range_seq start stop =
  let next i = if i > stop then None else Some (i, i + 1) in
  Seq.unfold next start
;;

let char_matrix lines =
  let x = lines.(0) |> String.to_array |> Array.length in
  let y = Array.length lines in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun y row ->
      let line = lines.(y) |> String.to_array in
      Array.mapi row ~f:(fun x _ -> line.(x)))
  in
  map, x, y
;;

let make_list_matrix lines ~(f : char -> _) =
  let x = lines.(0) |> String.to_array |> Array.length in
  let y = Array.length lines in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun y row ->
      let line = lines.(y) |> String.to_array in
      Array.mapi row ~f:(fun x _ -> f line.(x)) |> Array.to_list)
    |> Array.to_list
  in
  map, x, y
;;

let make_matrix_array lines ~(f : char -> _) =
  let x = lines.(0) |> String.to_array |> Array.length in
  let y = Array.length lines in
  let map =
    Array.make_matrix ~dimx:x ~dimy:y '.'
    |> Array.mapi ~f:(fun y row ->
      let line = lines.(y) |> String.to_array in
      Array.mapi row ~f:(fun x _ -> f line.(x)))
  in
  map, x, y
;;

let split_opt (list : 'a list) ~(on : 'a) ~(equal : 'a -> 'a -> bool) =
  let rec aux list acc out =
    match list with
    | [] -> out
    | [ last ] when equal last on ->
      let out =
        match acc with
        | [] -> out
        | x -> List.append out [ Some x ]
      in
      List.append out [ None ]
    | [ last ] ->
      let acc = List.append acc [ last ] in
      let out = List.append out [ Some acc ] in
      out
    | hd :: tail when equal hd on ->
      let out =
        match acc with
        | [] -> out
        | x -> List.append out [ Some x ]
      in
      let out = List.append out [ None ] in
      aux tail [] out
    | hd :: tail ->
      let acc = List.append acc [ hd ] in
      aux tail acc out
  in
  aux list [] []
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
  let newline = string "\n"
end
