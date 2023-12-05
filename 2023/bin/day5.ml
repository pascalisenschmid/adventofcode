open Core

module Converter = struct
  type t =
    { src : int
    ; dest : int
    ; length : int
    }

  let create_negative_ranges converters =
    let rec aux converters start out =
      match converters with
      | [] ->
        let neg_range =
          { src = start; dest = start; length = Int.max_value - start }
        in
        let out = List.append out [ neg_range ] in
        out
      | [ conv ] when conv.src > start ->
        let neg_range =
          { src = start; dest = start; length = conv.src - start }
        in
        let out = List.append out [ neg_range; conv ] in
        aux [] (conv.src + conv.length) out
      | [ conv ] ->
        let out = List.append out [ conv ] in
        aux [] (conv.src + conv.length) out
      | conv :: rest when conv.src > start ->
        let neg_range =
          { src = start; dest = start; length = conv.src - start }
        in
        let out = List.append out [ neg_range; conv ] in
        aux rest (conv.src + conv.length) out
      | conv :: rest ->
        let out = List.append out [ conv ] in
        aux rest (conv.src + conv.length) out
    in
    let sorted = List.sort converters ~compare:(fun f s -> f.src - s.src) in
    aux sorted 0 []
  ;;

  let parse lines =
    let rec aux lines out =
      match lines with
      | [] -> out, []
      | line :: rest when String.is_empty line -> out, rest
      | line :: rest when phys_equal (List.length (String.split ~on:' ' line)) 3
        ->
        let nums = String.split ~on:' ' line |> List.map ~f:Int.of_string in
        aux
          rest
          (List.append
             out
             [ { src = List.nth_exn nums 1
               ; dest = List.nth_exn nums 0
               ; length = List.nth_exn nums 2
               }
             ])
      | _ :: rest -> aux rest out
    in
    let converters, lines = aux lines [] in
    converters |> create_negative_ranges, lines
  ;;
end

module Seed = struct
  type t =
    { start : int
    ; length : int
    }

  let parse_seeds lines parser =
    let rec aux lines out =
      match lines with
      | line :: rest when String.is_empty line -> out, rest
      | line :: rest -> aux rest (parser (String.split line ~on:' ') out)
      | _ -> out, lines
    in
    aux lines []
  ;;

  let rec parse_single segments out =
    match segments with
    | [ seg ] when Advent.string_is_int seg ->
      List.append out [ { start = Int.of_string seg; length = 1 } ]
    | seg :: rest when Advent.string_is_int seg ->
      parse_single
        rest
        (List.append out [ { start = Int.of_string seg; length = 1 } ])
    | _ :: rest -> parse_single rest out
    | _ -> out
  ;;

  let rec parse_range segments out =
    match segments with
    | start :: len :: rest
      when Advent.string_is_int start && Advent.string_is_int len ->
      let start_int = Int.of_string start in
      let len_int = Int.of_string len in
      parse_range
        rest
        (List.append out [ { start = start_int; length = len_int } ])
    | _ :: rest -> parse_range rest out
    | _ -> out
  ;;
end

let rec walk
  (seed : Seed.t option list)
  (converters : Converter.t list)
  (out : Seed.t option list)
  =
  match seed with
  | Some seed :: rest ->
    let converter =
      List.find_exn converters ~f:(fun c ->
        c.src <= seed.start && c.src + c.length > seed.start)
    in
    let new_seed =
      Seed.
        { start = seed.start - converter.src + converter.dest
        ; length =
            Int.min seed.length (converter.length - (seed.start - converter.src))
        }
    in
    let out = out @ [ Some new_seed ] in
    let remaining_length = seed.length - new_seed.length in
    (match remaining_length with
     | x when x > 0 ->
       let remaining =
         Seed.
           { start = seed.start + seed.length - remaining_length
           ; length = remaining_length
           }
       in
       walk rest converters (walk [ Some remaining ] converters out)
     | _ -> walk rest converters out)
  | _ -> out
;;

let solve (seeds, converters_in_order) outpattern =
  List.map seeds ~f:(fun seed ->
    List.fold converters_in_order ~init:[ Some seed ] ~f:(fun acc converters ->
      walk acc converters []))
  |> List.join
  |> List.fold ~init:0 ~f:(fun acc seed ->
    match seed with
    | Some seed when seed.Seed.start < acc || acc = 0 -> seed.start
    | _ -> acc)
  |> sprintf outpattern
  |> print_endline
;;

let create_converters_in_order lines =
  let seed_to_soil, lines = Converter.parse lines in
  let soil_to_fert, lines = Converter.parse lines in
  let fert_to_water, lines = Converter.parse lines in
  let water_to_light, lines = Converter.parse lines in
  let light_to_temp, lines = Converter.parse lines in
  let temp_to_hum, lines = Converter.parse lines in
  let hum_to_loc, _ = Converter.parse lines in
  [ seed_to_soil
  ; soil_to_fert
  ; fert_to_water
  ; water_to_light
  ; light_to_temp
  ; temp_to_hum
  ; hum_to_loc
  ]
;;

let create_input lines parser =
  let seeds, lines = Seed.parse_seeds lines parser in
  seeds, create_converters_in_order lines
;;

let () =
  let lines = Advent.read_lines "./inputs/day5.txt" in
  let _ = solve (create_input lines Seed.parse_single) "Part 1: %d" in
  solve (create_input lines Seed.parse_range) "Part 2: %d"
;;
