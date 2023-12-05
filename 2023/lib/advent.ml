open Core

let read_lines file = In_channel.read_lines file

let string_is_int str =
  try
    int_of_string str |> ignore;
    true
  with
  | Failure _ -> false
;;
