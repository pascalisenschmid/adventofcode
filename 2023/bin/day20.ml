open Core
open Angstrom
module Map = Stdlib.Map
module Modjules = Map.Make (String)
module Inputs = Map.Make (String)

type mod_type =
  | Broadcaster
  | FlipFlop
  | Junction
[@@deriving eq, show]

let mod_type_of_string str =
  match str with
  | "" -> Broadcaster
  | "&" -> Junction
  | "%" -> FlipFlop
  | _ -> assert false
;;

type modjule =
  { receivers : string list
  ; value : int
  ; spec : mod_type
  ; inputs : int Inputs.t
  }

type message =
  { sender : string
  ; value : int
  ; receiver : string
  }
[@@deriving show]

let whitespace = take_while Char.is_whitespace

let parse_receiver =
  let* _ = whitespace in
  let* receiver = take_while1 (fun c -> not (equal_char ',' c)) in
  return receiver
;;

let parse_module =
  let* spec =
    take_while (fun c ->
      match c with
      | '%' | '&' -> true
      | _ -> false)
  in
  let* name = take_while1 (fun c -> not (Char.is_whitespace c)) in
  let* _ = string " -> " in
  let* receivers = sep_by1 (char ',') parse_receiver in
  return
    ( name
    , { receivers
      ; value = 0
      ; spec = mod_type_of_string spec
      ; inputs = Inputs.empty
      } )
;;

let parse_input lines =
  let rec aux lines out =
    match lines with
    | [] -> out
    | [ line ] ->
      (match parse_string ~consume:Prefix parse_module line with
       | Ok (name, receivers) -> Modjules.add name receivers out
       | Error _ -> assert false)
    | line :: rest ->
      let out = aux [ line ] out in
      aux rest out
  in
  aux lines Modjules.empty
;;

let fill_junctions (modjules : modjule Modjules.t) =
  let junctions =
    Modjules.filter (fun _ v -> equal_mod_type v.spec Junction) modjules
    |> Modjules.mapi (fun k v ->
      let inputs =
        Modjules.filter
          (fun _ m -> List.exists m.receivers ~f:(fun i -> equal_string i k))
          modjules
        |> Modjules.to_list
        |> List.map ~f:(fun (k, _) -> k)
        |> List.fold ~init:v.inputs ~f:(fun map name -> Inputs.add name 0 map)
      in
      { receivers = v.receivers; value = v.value; spec = v.spec; inputs })
  in
  Modjules.fold (fun k v acc -> Modjules.add k v acc) junctions modjules
;;

let part1 modjules =
  let rec aux queue modjules hi lo =
    match Queue.dequeue queue with
    | None -> modjules, hi, lo
    | Some message ->
      let hi, lo =
        match message.value with
        | 0 -> hi, lo + 1
        | 1 -> hi + 1, lo
        | _ -> assert false
      in
      let receiver = Modjules.find_opt message.receiver modjules in
      (match receiver with
       | None -> aux queue modjules hi lo
       | Some receiver ->
         let new_message, receiver =
           match receiver.spec, message.value with
           | Broadcaster, _ -> Some message.value, receiver
           | FlipFlop, 0 when receiver.value = 0 ->
             ( Some 1
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 1
               ; inputs = receiver.inputs
               } )
           | FlipFlop, 0 when receiver.value = 1 ->
             ( Some 0
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 0
               ; inputs = receiver.inputs
               } )
           | Junction, x ->
             let inputs = Inputs.add message.sender x receiver.inputs in
             let new_message =
               Inputs.fold
                 (fun _ v acc ->
                   match v with
                   | 0 -> 1
                   | _ -> acc)
                 inputs
                 0
             in
             ( Some new_message
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 0
               ; inputs
               } )
           | _ -> None, receiver
         in
         let modjules = Modjules.add message.receiver receiver modjules in
         let queue =
           match new_message with
           | None -> queue
           | Some value ->
             let new_messages =
               List.map receiver.receivers ~f:(fun r ->
                 { sender = message.receiver; value; receiver = r })
             in
             let _ = Queue.enqueue_all queue new_messages in
             queue
         in
         aux queue modjules hi lo)
  in
  let _, hi, lo =
    List.range 0 1000
    |> List.fold ~init:(modjules, 0, 0) ~f:(fun (modjules, hi, lo) _ ->
      let messages = Queue.create () in
      let _ =
        Queue.enqueue
          messages
          { sender = "button"; value = 0; receiver = "broadcaster" }
      in
      aux messages modjules hi lo)
  in
  hi * lo |> sprintf "Part 1: %d" |> print_endline
;;

let part2 modjules =
  let rec aux queue modjules presses found gate =
    match Queue.dequeue queue with
    | None -> modjules, presses, found
    | Some message
      when equal_string message.receiver "rg"
           && equal_string message.sender gate
           && message.value = 1 -> modjules, presses, true
    | Some message ->
      let receiver = Modjules.find_opt message.receiver modjules in
      (match receiver with
       | None -> aux queue modjules presses found gate
       | Some receiver ->
         let new_message, receiver =
           match receiver.spec, message.value with
           | Broadcaster, _ -> Some message.value, receiver
           | FlipFlop, 0 when receiver.value = 0 ->
             ( Some 1
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 1
               ; inputs = receiver.inputs
               } )
           | FlipFlop, 0 when receiver.value = 1 ->
             ( Some 0
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 0
               ; inputs = receiver.inputs
               } )
           | Junction, x ->
             let inputs = Inputs.add message.sender x receiver.inputs in
             let new_message =
               Inputs.fold
                 (fun _ v acc ->
                   match v with
                   | 0 -> 1
                   | _ -> acc)
                 inputs
                 0
             in
             ( Some new_message
             , { receivers = receiver.receivers
               ; spec = receiver.spec
               ; value = 0
               ; inputs
               } )
           | _ -> None, receiver
         in
         let modjules = Modjules.add message.receiver receiver modjules in
         let queue =
           match new_message with
           | None -> queue
           | Some value ->
             let new_messages =
               List.map receiver.receivers ~f:(fun r ->
                 { sender = message.receiver; value; receiver = r })
             in
             let _ = Queue.enqueue_all queue new_messages in
             queue
         in
         aux queue modjules presses found gate)
  in
  let rec loop modjules counter gate =
    let messages = Queue.create () in
    let _ =
      Queue.enqueue
        messages
        { sender = "button"; value = 0; receiver = "broadcaster" }
    in
    match aux messages modjules counter false gate with
    | m, c, false -> loop m (c + 1) gate
    | _, c, true -> c + 1
  in
  let one = loop modjules 0 "kd" in
  let two = loop modjules 0 "zf" in
  let three = loop modjules 0 "vg" in
  let four = loop modjules 0 "gs" in
  let lcm a b =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    a * b / gcd a b
  in
  one
  |> lcm two
  |> lcm three
  |> lcm four
  |> sprintf "Part 1: %d"
  |> print_endline
;;

let () =
  let modjules =
    Advent.read_lines "./inputs/day20.txt" |> parse_input |> fill_junctions
  in
  let _ = part1 modjules in
  part2 modjules
;;
