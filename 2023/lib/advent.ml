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
