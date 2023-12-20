open Core
open Angstrom
module Map = Stdlib.Map
module Workflows = Map.Make (String)

type category =
  | Xtreme
  | Musical
  | Aero
  | Shiny

type operation =
  | GT
  | LT

type rule =
  { cat : category
  ; op : operation
  ; num : int
  ; next : string
  }

type workflow =
  { rules : rule list
  ; next : string
  }

type part =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

let to_operation char =
  match char with
  | '>' -> GT
  | '<' -> LT
  | _ -> assert false
;;

let to_category char =
  match char with
  | 'x' -> Xtreme
  | 'm' -> Musical
  | 'a' -> Aero
  | 's' -> Shiny
  | _ -> assert false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let digit = take_while1 is_digit >>| Int.of_string

let parse_rule =
  let* cat = any_char in
  let* sep = any_char in
  let* num = digit in
  let* _ = char ':' in
  let* next = take_while1 (fun c -> not (equal_char c ',')) in
  return { cat = to_category cat; op = to_operation sep; num; next }
;;

let parse_workflow =
  let* name = take_while (fun c -> not (equal_char c '{')) in
  let* _ = char '{' in
  let* rules = sep_by1 (char ',') parse_rule in
  let* _ = char ',' in
  let* next = take_while1 (fun c -> not (equal_char c '}')) in
  return (name, { rules; next })
;;

let parse_dings =
  let* _ = any_char in
  let* _ = char '=' in
  let* num = digit in
  return num
;;

let parse_part =
  let* _ = char '{' in
  let* x = parse_dings in
  let* _ = char ',' in
  let* m = parse_dings in
  let* _ = char ',' in
  let* a = parse_dings in
  let* _ = char ',' in
  let* s = parse_dings in
  return { x; m; a; s }
;;

let parse_lines lines =
  let rec parse_parts lines workflows parts =
    match lines with
    | [] -> workflows, parts
    | [ line ] ->
      let result = parse_string ~consume:Prefix parse_part line in
      let parts =
        match result with
        | Ok part -> List.append parts [ part ]
        | Error _ -> assert false
      in
      workflows, parts
    | line :: rest ->
      let workflows, parts = parse_parts [ line ] workflows parts in
      parse_parts rest workflows parts
  in
  let rec parse_rules lines workflows =
    match lines with
    | [] -> assert false
    | [ _ ] -> assert false
    | line :: rest when Core.String.is_empty line ->
      parse_parts rest workflows []
    | line :: rest ->
      let result = parse_string ~consume:Prefix parse_workflow line in
      let workflows =
        match result with
        | Ok (name, workflow) -> Workflows.add name workflow workflows
        | Error _ -> assert false
      in
      parse_rules rest workflows
  in
  parse_rules lines Workflows.empty
;;

let part1 workflows parts =
  let rec eval (rules : rule list) fallback part =
    match rules with
    | [] -> fallback
    | [ rule ] ->
      (match rule.cat, rule.op with
       | Xtreme, GT when part.x > rule.num -> rule.next
       | Xtreme, LT when part.x < rule.num -> rule.next
       | Musical, GT when part.m > rule.num -> rule.next
       | Musical, LT when part.m < rule.num -> rule.next
       | Aero, GT when part.a > rule.num -> rule.next
       | Aero, LT when part.a < rule.num -> rule.next
       | Shiny, GT when part.s > rule.num -> rule.next
       | Shiny, LT when part.s < rule.num -> rule.next
       | _ -> eval [] fallback part)
    | rule :: rest ->
      (match rule.cat, rule.op with
       | Xtreme, GT when part.x > rule.num -> rule.next
       | Xtreme, LT when part.x < rule.num -> rule.next
       | Musical, GT when part.m > rule.num -> rule.next
       | Musical, LT when part.m < rule.num -> rule.next
       | Aero, GT when part.a > rule.num -> rule.next
       | Aero, LT when part.a < rule.num -> rule.next
       | Shiny, GT when part.s > rule.num -> rule.next
       | Shiny, LT when part.s < rule.num -> rule.next
       | _ -> eval rest fallback part)
  in
  List.map parts ~f:(fun part ->
    let rec find part workflow =
      match workflow with
      | "R" -> 0
      | "A" -> part.x + part.m + part.a + part.s
      | wf ->
        let wf = Workflows.find wf workflows in
        let res = eval wf.rules wf.next part in
        find part res
    in
    find part "in")
  |> List.fold ~init:0 ~f:( + )
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let part2 workflows =
  let rec aux wf minmax =
    let sum =
      match wf with
      | "R" ->
        let (xmin, xmax), (mmin, mmax), (amin, amax), (smin, smax) = minmax in
        (xmax - xmin + 1)
        * (mmax - mmin + 1)
        * (amax - amin + 1)
        * (smax - smin + 1)
      | "A" -> 0
      | wf ->
        let wf = Workflows.find wf workflows in
        let sum, minmax =
          List.fold
            wf.rules
            ~init:(0, minmax)
            ~f:(fun (acc, (x, m, a, s)) rule ->
              match rule.cat, rule.op with
              | Xtreme, GT ->
                let min, max = x in
                let newmin = Int.max (rule.num + 1) min in
                let newmax = Int.min rule.num max in
                let res = aux rule.next ((newmin, max), m, a, s) in
                acc + res, ((min, newmax), m, a, s)
              | Xtreme, LT ->
                let min, max = x in
                let newmax = Int.min (rule.num - 1) max in
                let newmin = Int.max rule.num min in
                let res = aux rule.next ((min, newmax), m, a, s) in
                acc + res, ((newmin, max), m, a, s)
              | Musical, GT ->
                let min, max = m in
                let newmin = Int.max (rule.num + 1) min in
                let newmax = Int.min rule.num max in
                let res = aux rule.next (x, (newmin, max), a, s) in
                acc + res, (x, (min, newmax), a, s)
              | Musical, LT ->
                let min, max = m in
                let newmax = Int.min (rule.num - 1) max in
                let newmin = Int.max rule.num min in
                let res = aux rule.next (x, (min, newmax), a, s) in
                acc + res, (x, (newmin, max), a, s)
              | Aero, GT ->
                let min, max = a in
                let newmin = Int.max (rule.num + 1) min in
                let newmax = Int.min rule.num max in
                let res = aux rule.next (x, m, (newmin, max), s) in
                acc + res, (x, m, (min, newmax), s)
              | Aero, LT ->
                let min, max = a in
                let newmax = Int.min (rule.num - 1) max in
                let newmin = Int.max rule.num min in
                let res = aux rule.next (x, m, (min, newmax), s) in
                acc + res, (x, m, (newmin, max), s)
              | Shiny, GT ->
                let min, max = s in
                let newmin = Int.max (rule.num + 1) min in
                let newmax = Int.min rule.num max in
                let res = aux rule.next (x, m, a, (newmin, max)) in
                acc + res, (x, m, a, (min, newmax))
              | Shiny, LT ->
                let min, max = s in
                let newmax = Int.min (rule.num - 1) max in
                let newmin = Int.max rule.num min in
                let res = aux rule.next (x, m, a, (min, newmax)) in
                acc + res, (x, m, a, (newmin, max)))
        in
        sum + aux wf.next minmax
    in
    sum
  in
  let supermax = 4000 * 4000 * 4000 * 4000 in
  aux "in" ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
  |> Int.( - ) supermax
  |> sprintf "Part 2: %d"
  |> print_endline
;;

let () =
  let lines = Advent.read_lines "./inputs/day19.txt" in
  let workflows, parts = parse_lines lines in
  let _ = part1 workflows parts in
  part2 workflows
;;
