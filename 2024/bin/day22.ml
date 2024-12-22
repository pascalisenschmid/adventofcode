open Core

module Map = Stdlib.Map.Make (struct
    type t = int * int * int * int [@@deriving compare]
  end)

module Set = Stdlib.Set.Make (struct
    type t = int * int * int * int [@@deriving compare]
  end)

let prune n = n % 16777216
let mix = Int.bit_xor

let next n =
  let n = mix n (n * 64) |> prune in
  let n = mix n (n / 32) |> prune in
  mix n (n * 2048) |> prune
;;

let last n = n % 10

let pnd n =
  let prev_price = last n in
  let n = next n in
  let price = last n in
  let change = price - prev_price in
  price, change, n
;;

let update_if_unseen map seen key price =
  match Set.mem key seen with
  | true -> map, seen
  | false ->
    ( Map.update
        key
        (function
          | Some b -> Some (b + price)
          | None -> Some price)
        map
    , Set.add key seen )
;;

let calculate times n =
  let rec calc n = function
    | x when x > times -> n
    | x -> calc (next n) (x + 1)
  in
  calc n 1
;;

let calculate2 times acc init =
  let rec calc n acc acc2 seen c =
    match c, acc2 with
    | x, _ when x > times -> acc
    | x, [ x3; x2; x1 ] ->
      let price, change, n = pnd n in
      let key = x1, x2, x3, change in
      let acc, seen = update_if_unseen acc seen key price in
      calc n acc [ change; x3; x2 ] seen (x + 1)
    | x, _ ->
      let _, change, n = pnd n in
      calc n acc (change :: acc2) seen (x + 1)
  in
  calc init acc [] Set.empty 1
;;

let () =
  let calculate = calculate 2000 in
  Advent.read_lines "inputs/day22.txt"
  |> List.map ~f:Int.of_string
  |> List.map ~f:calculate
  |> List.fold ~init:0 ~f:Int.( + )
  |> Fmt.pr "\nPart 1: %d"
;;

let () =
  let calculate = calculate2 2000 in
  Advent.read_lines "inputs/day22.txt"
  |> List.map ~f:Int.of_string
  |> List.fold ~init:Map.empty ~f:(fun acc n -> calculate acc n)
  |> Map.to_list
  |> List.map ~f:(fun (_, v) -> v)
  |> List.fold ~init:0 ~f:Int.max
  |> Fmt.pr "\nPart 2: %d"
;;
