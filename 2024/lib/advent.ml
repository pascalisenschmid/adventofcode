open Core

let read_lines file = In_channel.read_lines file
let read_all file = In_channel.read_all file
let read_lines_seq file = read_lines file |> Stdlib.List.to_seq

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
