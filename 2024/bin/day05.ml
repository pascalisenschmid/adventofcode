open Core
module Rules = Stdlib.Map.Make (Int)

let parse_rules lines =
  List.fold
    ~init:Rules.empty
    ~f:(fun acc line ->
      let split = String.split ~on:'|' line in
      let l, r = List.hd_exn split, List.last_exn split in
      let l, r = Int.of_string l, Int.of_string r in
      acc
      |> Rules.update l (function
        | None -> Some [ r ]
        | Some rule -> Some (List.append rule [ r ])))
    lines
;;

let parse_sequence line = String.split ~on:',' line |> List.map ~f:Int.of_string

let check_sequence rules sequence =
  List.mapi sequence ~f:(fun cursor num ->
    List.foldi sequence ~init:true ~f:(fun idx acc other ->
      match acc with
      | false -> false
      | true ->
        (match other with
         | _ when idx < cursor ->
           (match Rules.find_opt other rules with
            | Some rule when List.exists rule ~f:(fun item -> item = num) ->
              true
            | _ -> false)
         | _ when idx > cursor ->
           (match Rules.find_opt num rules with
            | Some rule when List.exists rule ~f:(fun item -> item = other) ->
              true
            | _ -> false)
         | _ -> acc)))
  |> List.exists ~f:(fun item -> Bool.( = ) item false)
  |> not
;;

let compare_with_rules rules left right =
  match Rules.find_opt left rules with
  | Some rule when List.exists rule ~f:(fun item -> item = right) -> -1
  | _ -> 1
;;

let rec middle = function
  | [] -> failwith "omegalul"
  | [ x ] -> x
  | _ :: tl -> middle (List.tl_exn (List.rev tl))
;;

let () =
  let blocks =
    Advent.read_lines "inputs/day05.txt" |> Advent.split_on_empty_line
  in
  let rules, sequences = List.hd_exn blocks, List.last_exn blocks in
  let rules = parse_rules rules in
  List.map sequences ~f:parse_sequence
  |> List.filter ~f:(fun sequence -> sequence |> check_sequence rules)
  |> List.map ~f:middle
  |> List.fold ~init:0 ~f:Int.( + )
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let () =
  let blocks =
    Advent.read_lines "inputs/day05.txt" |> Advent.split_on_empty_line
  in
  let rules, sequences = List.hd_exn blocks, List.last_exn blocks in
  let rules = parse_rules rules in
  let compare_with_rules = compare_with_rules rules in
  List.map sequences ~f:parse_sequence
  |> List.filter ~f:(fun sequence -> sequence |> check_sequence rules |> not)
  |> List.map ~f:(fun seq -> List.sort seq ~compare:compare_with_rules)
  |> List.map ~f:middle
  |> List.fold ~init:0 ~f:Int.( + )
  |> sprintf "Part 2: %d"
  |> print_endline
;;
