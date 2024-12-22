open Core

type coord =
  { x : int
  ; y : int
  }
[@@deriving equal, show, compare]

module Map = Stdlib.Map.Make (struct
    type t = coord * coord * int [@@deriving compare]
  end)

type move =
  | Left
  | Right
  | Down
  | Up
  | Press
[@@deriving equal, show]

module Keypad = struct
  let parse = function
    | '7' -> { x = 0; y = 0 }
    | '8' -> { x = 0; y = 1 }
    | '9' -> { x = 0; y = 2 }
    | '4' -> { x = 1; y = 0 }
    | '5' -> { x = 1; y = 1 }
    | '6' -> { x = 1; y = 2 }
    | '1' -> { x = 2; y = 0 }
    | '2' -> { x = 2; y = 1 }
    | '3' -> { x = 2; y = 2 }
    | '0' -> { x = 3; y = 1 }
    | _ -> { x = 3; y = 2 }
  ;;

  let move c = function
    | Up -> { x = c.x - 1; y = c.y }
    | Right -> { x = c.x; y = c.y + 1 }
    | Down -> { x = c.x + 1; y = c.y }
    | Left -> { x = c.x; y = c.y - 1 }
    | _ -> failwith "cant move while pressing"
  ;;

  let input keys =
    let curr = ref { x = 3; y = 2 } in
    let positions = String.to_list keys |> List.map ~f:parse in
    List.concat_map positions ~f:(fun goal ->
      let rec aux goal curr acc =
        match goal, curr, acc with
        | g, c, _ when equal_coord g c -> List.rev (Press :: acc)
        | g, c, _ when g.y = 0 && c.x = 3 && g.x < c.x ->
          aux goal (move curr Up) (Up :: acc)
        | g, c, Up :: _ when g.y = 0 && g.x < c.x ->
          aux goal (move curr Up) (Up :: acc)
        | g, c, _ when g.x = 3 && c.y = 0 ->
          aux goal (move curr Right) (Right :: acc)
        | g, c, Right :: _ when g.x = 3 && c.y < g.y ->
          aux goal (move curr Right) (Right :: acc)
        | g, c, _ when g.y < c.y -> aux goal (move curr Left) (Left :: acc)
        | g, c, _ when g.x < c.x -> aux goal (move curr Up) (Up :: acc)
        | g, c, _ when g.x > c.x -> aux goal (move curr Down) (Down :: acc)
        | g, c, _ when g.y > c.y -> aux goal (move curr Right) (Right :: acc)
        | _ -> failwith "bla"
      in
      let out = aux goal !curr [] in
      curr := goal;
      out)
  ;;
end

module Dirpad = struct
  let parse = function
    | Up -> { x = 0; y = 1 }
    | Press -> { x = 0; y = 2 }
    | Left -> { x = 1; y = 0 }
    | Down -> { x = 1; y = 1 }
    | Right -> { x = 1; y = 2 }
  ;;

  let move c = function
    | Up -> { x = c.x - 1; y = c.y }
    | Right -> { x = c.x; y = c.y + 1 }
    | Down -> { x = c.x + 1; y = c.y }
    | Left -> { x = c.x; y = c.y - 1 }
    | _ -> failwith "cant move while pressing"
  ;;

  let rec get_moves goal curr acc =
    match goal, curr, acc with
    | g, c, _ when equal_coord g c -> List.rev (Press :: acc)
    | g, c, _ when equal_coord g { x = 1; y = 0 } && g.x > c.x ->
      get_moves goal (move curr Down) (Down :: acc)
    | _, c, _ when equal_coord c { x = 1; y = 0 } ->
      get_moves goal (move curr Right) (Right :: acc)
    | g, c, Right :: _ when g.x < c.x && g.y > c.y ->
      get_moves goal (move curr Right) (Right :: acc)
    | g, c, _ when g.y < c.y -> get_moves goal (move curr Left) (Left :: acc)
    | g, c, _ when g.x < c.x -> get_moves goal (move curr Up) (Up :: acc)
    | g, c, _ when g.x > c.x -> get_moves goal (move curr Down) (Down :: acc)
    | g, c, _ when g.y > c.y -> get_moves goal (move curr Right) (Right :: acc)
    | _ -> failwith "nop"
  ;;

  let input2 depth moves =
    let rec aux moves depth cache =
      match depth with
      | 0 -> List.length moves, cache
      | _ ->
        let cost, _, cache =
          List.fold
            moves
            ~init:(0, { x = 0; y = 2 }, cache)
            ~f:(fun (len, curr, cache) goal ->
              let key = curr, goal, depth in
              match Map.mem key cache with
              | true -> len + Map.find key cache, goal, cache
              | false ->
                let moves = get_moves goal curr [] |> List.map ~f:parse in
                let cost, cache = aux moves (depth - 1) cache in
                let cache = Map.add key cost cache in
                len + cost, goal, cache)
        in
        cost, cache
    in
    let moves = List.map moves ~f:parse in
    let cost, _ = aux moves depth Map.empty in
    cost
  ;;
end

let solve input depth =
  Advent.read_lines input
  |> List.map ~f:(fun input ->
    let moves = Keypad.input input |> Dirpad.input2 depth in
    let num = String.drop_suffix input 1 |> Int.of_string in
    moves * num)
  |> List.fold ~init:0 ~f:Int.( + )
;;

let () = solve "inputs/day21.txt" 2 |> Fmt.pr "\nPart 1 %d"
let () = solve "inputs/day21.txt" 25 |> Fmt.pr "\nPart 2 %d"
