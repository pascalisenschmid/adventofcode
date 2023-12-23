open Core
open Advent.Parser

module Coordinates = struct
  type point3d =
    { x : int
    ; y : int
    ; z : int
    }
  [@@deriving show, compare]

  let parse_point =
    let* x = digit <* char ',' in
    let* y = digit <* char ',' in
    let* z = digit in
    return { x; y; z }
  ;;

  let drop p3d = { x = p3d.x; y = p3d.y; z = p3d.z - 1 }
end

module Brick = struct
  include Coordinates

  type t =
    { head : point3d
    ; tail : point3d
    ; all : point3d list
    }
  [@@deriving show, compare]

  let expand hd tail =
    let rec aux curr tail out =
      match curr with
      | c when c.x < tail.x ->
        let out =
          List.append out [ Coordinates.{ x = c.x; y = c.y; z = c.z } ]
        in
        let x, y, z = c.x + 1, c.y, c.z in
        aux Coordinates.{ x; y; z } tail out
      | c when c.y < tail.y ->
        let out =
          List.append out [ Coordinates.{ x = c.x; y = c.y; z = c.z } ]
        in
        let x, y, z = c.x, c.y + 1, c.z in
        aux Coordinates.{ x; y; z } tail out
      | c when c.z < tail.z ->
        let out =
          List.append out [ Coordinates.{ x = c.x; y = c.y; z = c.z } ]
        in
        let x, y, z = c.x, c.y, c.z + 1 in
        aux Coordinates.{ x; y; z } tail out
      | _ -> List.append out [ tail ]
    in
    aux hd tail []
  ;;

  let parse_brick =
    let* head = parse_point in
    let* _ = char '~' in
    let* tail = parse_point in
    let all = expand head tail in
    return { head; tail; all }
  ;;

  let parse_bricks = sep_by1 newline parse_brick
  let on_ground brick = List.exists brick.all ~f:(fun c -> c.z = 1)
end

open Brick
module IntMap = Stdlib.Map.Make (Int)

let make_map bricks =
  let _, bricks =
    List.fold bricks ~init:(1, IntMap.empty) ~f:(fun (idx, map) brick ->
      let map = IntMap.add idx brick map in
      let idx = idx + 1 in
      idx, map)
  in
  bricks
;;

let find_bricks_below (bricks : Brick.t IntMap.t) key (brick : Brick.t) =
  bricks
  |> IntMap.filter (fun k other ->
    if k = key
    then false
    else
      List.fold other.all ~init:false ~f:(fun acc other_coord ->
        let r =
          List.exists brick.all ~f:(fun my_coord ->
            my_coord.x = other_coord.x
            && my_coord.y = other_coord.y
            && my_coord.z - 1 = other_coord.z)
        in
        match r with
        | false -> acc
        | true -> true))
;;

let find_bricks_above (bricks : Brick.t IntMap.t) key (brick : Brick.t) =
  bricks
  |> IntMap.filter (fun k other ->
    if k = key
    then false
    else
      List.fold other.all ~init:false ~f:(fun acc other_coord ->
        let r =
          List.exists brick.all ~f:(fun my_coord ->
            my_coord.x = other_coord.x
            && my_coord.y = other_coord.y
            && my_coord.z + 1 = other_coord.z)
        in
        match r with
        | false -> acc
        | true -> true))
;;

let drop_brick bricks (key, (brick : Brick.t)) =
  let rec aux count brick =
    match find_bricks_below bricks key brick |> IntMap.cardinal with
    | 0 ->
      if Brick.on_ground brick
      then count, brick
      else (
        let head = Coordinates.drop brick.head in
        let tail = Coordinates.drop brick.tail in
        let all = expand head tail in
        aux (count + 1) Brick.{ head; tail; all })
    | _ -> count, brick
  in
  match aux 0 brick with
  | 0, brick -> false, brick
  | _, brick -> true, brick
;;

let drop_bricks bricks =
  let rec aux drop bricks =
    match drop with
    | false -> bricks
    | true ->
      let drop, new_bricks =
        IntMap.fold
          (fun k v (did_drop, new_bricks) ->
            let dropped, new_brick = drop_brick bricks (k, v) in
            let new_bricks = IntMap.add k new_brick new_bricks in
            if dropped then true, new_bricks else did_drop, new_bricks)
          bricks
          (false, IntMap.empty)
      in
      aux drop new_bricks
  in
  let result = aux true bricks in
  result
;;

let find_supporting (bricks : Brick.t IntMap.t) key (brick : Brick.t) acc =
  let supported_by =
    find_bricks_below bricks key brick
    |> IntMap.to_list
    |> List.map ~f:(fun (k, v) -> k)
  in
  let supporting =
    find_bricks_above bricks key brick
    |> IntMap.to_list
    |> List.map ~f:(fun (k, v) -> k)
  in
  IntMap.add key (supported_by, supporting) acc
;;

let build_support_map bricks =
  let find_supporting = find_supporting bricks in
  IntMap.fold find_supporting bricks IntMap.empty
;;

let part1 support_map =
  let destroyable =
    IntMap.fold
      (fun k (below, above) destroyable ->
        let destroyable =
          match List.length above with
          | 0 -> IntMap.add k true destroyable
          | _ -> destroyable
        in
        match List.length below with
        | 0 -> destroyable
        | 1 -> IntMap.add (List.hd_exn below) false destroyable
        | _ ->
          List.fold below ~init:destroyable ~f:(fun acc k ->
            let exists = IntMap.find_opt k destroyable in
            match exists with
            | None -> IntMap.add k true acc
            | Some x -> acc))
      support_map
      IntMap.empty
  in
  destroyable
  |> IntMap.filter (fun _ v -> v)
  |> IntMap.cardinal
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let part2 (support_map : (int list * int list) IntMap.t) =
  let rec aux (map : (int list * int list) IntMap.t) queue first dropping =
    let curr = Queue.dequeue queue in
    match curr, first with
    | None, _ -> dropping
    | Some brick, true ->
      let dropping = IntMap.add brick true dropping in
      let below, above = IntMap.find brick map in
      let _ = Queue.enqueue_all queue above in
      aux map queue false dropping
    | Some brick, false ->
      let below, above = IntMap.find brick map in
      let all_below_destroyed =
        List.fold below ~init:true ~f:(fun acc b ->
          match IntMap.find_opt b dropping with
          | None -> false
          | Some _ -> acc)
      in
      if all_below_destroyed
      then (
        let dropping = IntMap.add brick true dropping in
        let _ = Queue.enqueue_all queue above in
        aux map queue false dropping)
      else aux map queue false dropping
  in
  let counts =
    support_map
    |> IntMap.mapi (fun k sup ->
      let queue = Queue.create () in
      let _ = Queue.enqueue queue k in
      let result = aux support_map queue true IntMap.empty |> IntMap.cardinal in
      result - 1)
    |> IntMap.to_list
  in
  let counts = counts |> List.map ~f:(fun (k, v) -> v) in
  let max = List.fold ~init:0 ~f:Int.( + ) counts in
  sprintf "Part 2: %d" max |> print_endline
;;

let () =
  let line = Advent.read_all "./inputs/day22.txt" in
  let result = parse_string ~consume:Prefix parse_bricks line in
  let _ = print_endline "parsed" in
  let support_map =
    match result with
    | Ok bricks ->
      List.sort bricks ~compare:Brick.compare
      |> make_map
      |> drop_bricks
      |> build_support_map
    | Error _ -> assert false
  in
  let _ = print_endline "done" in
  let _ = part1 support_map in
  part2 support_map
;;
