open Core
open Advent

module Map = Stdlib.Map.Make (struct
    type t = Matrix.coord [@@deriving compare]
  end)

type direction =
  | Left
  | Up
  | Right
  | Down
[@@deriving show, equal, compare]

let next (coord : Advent.Matrix.coord) dir =
  let open Advent.Matrix in
  match dir with
  | Left -> { x = coord.x; y = coord.y - 1 }
  | Up -> { x = coord.x - 1; y = coord.y }
  | Right -> { x = coord.x; y = coord.y + 1 }
  | Down -> { x = coord.x + 1; y = coord.y }
;;

let parse line =
  let split = String.split line ~on:',' in
  let x = List.last_exn split |> Int.of_string in
  let y = List.hd_exn split |> Int.of_string in
  Matrix.{ x; y }
;;

let dijkstra start goal matrix =
  let try_insert queue cost coord =
    match Matrix.get_opt matrix coord with
    | Some '.' -> Queue.enqueue queue (coord, cost)
    | _ -> ()
  in
  let rec aux queue map =
    match Queue.dequeue queue with
    | None -> map
    | Some (coord, cost) ->
      (match Map.find_opt coord map with
       | Some c when c <= cost -> aux queue map
       | _ ->
         let map = Map.add coord cost map in
         next coord Up |> try_insert queue (cost + 1);
         next coord Down |> try_insert queue (cost + 1);
         next coord Left |> try_insert queue (cost + 1);
         next coord Right |> try_insert queue (cost + 1);
         aux queue map)
  in
  let queue = Queue.create () in
  Queue.enqueue queue (start, 0);
  aux queue Map.empty |> Map.find_opt goal
;;

let part1 input dim (bytes : int) goal =
  let matrix = Array.make_matrix ~dimx:dim ~dimy:dim '.' in
  let seq = Advent.read_seq input |> Sequence.map ~f:parse in
  let seq = Sequence.take seq bytes in
  let matrix =
    Sequence.fold seq ~init:matrix ~f:(fun acc coord ->
      acc.(coord.x).(coord.y) <- '#';
      acc)
  in
  dijkstra Matrix.{ x = 0; y = 0 } goal matrix |> Option.value_exn
;;

let part2 input dim initial goal =
  let matrix = Array.make_matrix ~dimx:dim ~dimy:dim '.' in
  let start = Matrix.{ x = 0; y = 0 } in
  let result =
    Advent.read_seq input
    |> Sequence.map ~f:parse
    |> Sequence.fold_until
         ~init:(matrix, 1)
         ~f:(fun (matrix, byte) coord ->
           matrix.(coord.x).(coord.y) <- '#';
           if byte <= initial
           then Continue (matrix, byte + 1)
           else (
             match dijkstra start goal matrix with
             | Some _ -> Continue (matrix, byte + 1)
             | None -> Some coord |> Stop))
         ~finish:(fun _ -> None)
  in
  match result with
  | Some c -> sprintf "%d,%d" c.y c.x
  | None -> failwith "noob"
;;

let () =
  part1 "inputs/day18-test.txt" 7 12 Matrix.{ x = 6; y = 6 }
  |> Fmt.pr "\nSample 1: %d"
;;

let () =
  part1 "inputs/day18.txt" 71 1024 Matrix.{ x = 70; y = 70 }
  |> Fmt.pr "\nPart 1: %d"
;;

let () =
  part2 "inputs/day18-test.txt" 7 12 Matrix.{ x = 6; y = 6 }
  |> Fmt.pr "\nSample 2: %s"
;;

let () =
  part2 "inputs/day18.txt" 71 1024 Matrix.{ x = 70; y = 70 }
  |> Fmt.pr "\nPart 2: %s"
;;
