open Core

let read_lines file = In_channel.read_lines file

let string_is_int str =
  try
    int_of_string str |> ignore;
    true
  with
  | Failure _ -> false
;;

let print_string_list list = List.iter list ~f:print_endline
let compare_desc a b = compare a b * -1

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
