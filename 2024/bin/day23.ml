open Core

let populate_set from to' map =
  let update x = function
    | None -> Set.singleton (module String) x
    | Some set -> Set.add set x
  in
  Hashtbl.update map from ~f:(update to');
  Hashtbl.update map to' ~f:(update from);
  map
;;

let parse_input lines =
  lines
  |> Sequence.of_list
  |> Sequence.map ~f:(fun line ->
    match String.split line ~on:'-' with
    | [ from; to' ] -> from, to'
    | _ -> failwith "no parse")
  |> Sequence.fold
       ~init:(Hashtbl.create (module String))
       ~f:(fun acc (from, to') ->
         acc |> populate_set from to' |> populate_set to' from)
;;

let find_tiplets edges =
  Hashtbl.fold
    edges
    ~init:(Set.empty (module String))
    ~f:(fun ~key:from ~data:to' acc ->
      Set.fold to' ~init:acc ~f:(fun acc t ->
        let subset = Hashtbl.find_exn edges t in
        Set.fold subset ~init:acc ~f:(fun acc t' ->
          match t' with
          | t' when equal_string t' t -> acc
          | t' when equal_string t' from -> acc
          | t' when Set.mem to' t' ->
            List.sort [ from; t; t' ] ~compare:String.compare
            |> String.concat ~sep:","
            |> Set.add acc
          | _ -> acc)))
;;

let filter_t triplets =
  let t_prefix s = String.is_prefix s ~prefix:"t" in
  Set.filter
    ~f:(fun t ->
      match String.split t ~on:',' with
      | [ p1; p2; p3 ] when t_prefix p1 || t_prefix p2 || t_prefix p3 -> true
      | _ -> false)
    triplets
;;

let find_connected edges =
  Hashtbl.fold
    edges
    ~init:(Set.empty (module String))
    ~f:(fun ~key:from ~data:to' acc ->
      let temp = Set.add to' from in
      Set.fold to' ~init:temp ~f:(fun acc t ->
        match Set.mem acc t with
        | false -> acc
        | true -> Set.add (Hashtbl.find_exn edges t) t |> Set.inter acc)
      |> Set.to_list
      |> List.sort ~compare:String.compare
      |> String.concat ~sep:","
      |> Set.add acc)
  |> Set.to_list
;;

let longest acc s = if String.length s > String.length acc then s else acc

let () =
  Advent.read_lines "inputs/day23.txt"
  |> parse_input
  |> find_tiplets
  |> filter_t
  |> Set.length
  |> Fmt.pr "\nPart 1: %d"
;;

let () =
  Advent.read_lines "inputs/day23.txt"
  |> parse_input
  |> find_connected
  |> List.reduce_exn ~f:longest
  |> Fmt.pr "\nPart 2: %s"
;;
