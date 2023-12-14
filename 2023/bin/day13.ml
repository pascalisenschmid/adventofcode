open Core

let accumulate_blocks lines =
  let rec aux lines acc out =
    match lines with
    | [] -> out
    | [ line ] ->
      let acc = List.append acc [ line ] in
      let out = List.append out [ acc ] in
      out
    | line :: rest when String.length line = 0 ->
      let out = List.append out [ acc ] in
      aux rest [] out
    | line :: rest ->
      let acc = List.append acc [ line ] in
      aux rest acc out
  in
  aux lines [] []
;;

let rec mirroring lines rev_acc =
  match lines, rev_acc with
  | [ l ], [ a ] when equal_string l a -> true
  | [ l ], a :: _ when equal_string l a -> true
  | l :: _, [ a ] when equal_string l a -> true
  | l :: lrest, a :: arest when equal_string l a -> mirroring lrest arest
  | _ -> false
;;

let smudges lines rev_acc =
  let one_different line1 line2 =
    let diff =
      List.fold2_exn
        (String.to_list line1)
        (String.to_list line2)
        ~init:0
        ~f:(fun acc c1 c2 ->
          match equal_char c1 c2 with
          | true -> acc
          | false -> acc + 1)
    in
    diff = 1
  in
  let rec aux lines rev_acc switched =
    match lines, rev_acc, switched with
    | [ l ], [ a ], true when equal_string l a -> true
    | [ l ], [ a ], false when equal_string l a -> false
    | [ _ ], [ _ ], true -> false
    | [ l ], [ a ], false ->
      (match one_different l a with
       | true -> true
       | false -> false)
    | l :: _, [ a ], true when equal_string l a -> true
    | l :: _, [ a ], false when equal_string l a -> false
    | _ :: _, [ _ ], true -> false
    | l :: _, [ a ], false ->
      (match one_different l a with
       | true -> true
       | false -> false)
    | [ l ], a :: _, true when equal_string l a -> true
    | [ l ], a :: _, false when equal_string l a -> false
    | [ _ ], _ :: _, true -> false
    | [ l ], a :: _, false ->
      (match one_different l a with
       | true -> true
       | false -> false)
    | l :: lrest, a :: arest, true when equal_string l a -> aux lrest arest true
    | l :: lrest, a :: arest, false when equal_string l a ->
      aux lrest arest false
    | _ :: _, _ :: _, true -> false
    | l :: lrest, a :: arest, false ->
      (match one_different l a with
       | true -> aux lrest arest true
       | false -> false)
    | _ -> false
  in
  aux lines rev_acc false
;;

let find_mirror mirrorfn fx lines =
  let rec aux (lines : string list) idx acc =
    match lines with
    | [] -> 0
    | [ line ] ->
      (match List.last acc with
       | None -> 0
       | Some _ ->
         let is_mirror = mirrorfn [ line ] (List.rev acc) in
         (match is_mirror with
          | true -> fx idx
          | false -> 0))
    | line :: rest ->
      (match List.last acc with
       | None -> aux rest (idx + 1) (List.append acc [ line ])
       | Some _ ->
         let is_mirror = mirrorfn (List.append [ line ] rest) (List.rev acc) in
         (match is_mirror with
          | false -> aux rest (idx + 1) (List.append acc [ line ])
          | true -> fx idx))
  in
  aux lines 0 []
;;

let make_vertical_lines lines =
  let length = String.length (List.nth_exn lines 0) in
  let lines = Array.of_list lines in
  List.range 0 length
  |> List.map ~f:(fun i ->
    Array.map lines ~f:(fun line ->
      let x = String.to_array line in
      x.(i))
    |> String.of_array)
;;

let hfx idx = idx * 100
let vfx idx = idx

let () =
  let blocks = Advent.read_lines "./inputs/day13.txt" |> accumulate_blocks in
  let _ = print_endline "" in
  let _ =
    List.fold blocks ~init:0 ~f:(fun acc block ->
      let h = find_mirror mirroring hfx block in
      let v = block |> make_vertical_lines |> find_mirror mirroring vfx in
      acc + h + v)
    |> sprintf "Part 1: %d"
    |> print_endline
  in
  List.fold blocks ~init:0 ~f:(fun acc block ->
    let h = find_mirror smudges hfx block in
    let v = block |> make_vertical_lines |> find_mirror smudges vfx in
    acc + h + v)
  |> sprintf "Part 2: %d"
  |> print_endline
;;
