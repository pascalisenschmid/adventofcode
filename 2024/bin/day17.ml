open Core

module Computer = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    ; instructions : int array
    ; pointer : int
    ; output : int list
    }
  [@@deriving show]

  let adv combo comp =
    { comp with a = comp.a / Int.pow 2 combo; pointer = comp.pointer + 2 }
  ;;

  let bxl literal comp =
    { comp with b = Int.bit_xor comp.b literal; pointer = comp.pointer + 2 }
  ;;

  let bst combo comp = { comp with b = combo % 8; pointer = comp.pointer + 2 }

  let jnz literal comp =
    match comp.a with
    | 0 -> { comp with pointer = comp.pointer + 2 }
    | _ -> { comp with pointer = literal }
  ;;

  let bxc comp =
    { comp with b = Int.bit_xor comp.b comp.c; pointer = comp.pointer + 2 }
  ;;

  let out combo comp =
    let result = combo % 8 in
    let output =
      Int.to_string result |> String.to_list |> List.map ~f:Char.get_digit_exn
    in
    { comp with
      output = List.append comp.output output
    ; pointer = comp.pointer + 2
    }
  ;;

  let bdv combo comp =
    { comp with b = comp.a / Int.pow 2 combo; pointer = comp.pointer + 2 }
  ;;

  let cdv combo comp =
    { comp with c = comp.a / Int.pow 2 combo; pointer = comp.pointer + 2 }
  ;;

  let execute opcode literal comp =
    let combo =
      match literal with
      | 4 -> comp.a
      | 5 -> comp.b
      | 6 -> comp.c
      | x when x <= 3 -> x
      | x -> sprintf "invald literal %d" x |> Invalid_argument |> raise
    in
    match opcode with
    | 0 -> adv combo comp
    | 1 -> bxl literal comp
    | 2 -> bst combo comp
    | 3 -> jnz literal comp
    | 4 -> bxc comp
    | 5 -> out combo comp
    | 6 -> bdv combo comp
    | 7 -> cdv combo comp
    | x -> sprintf "invald opcode %d" x |> Invalid_argument |> raise
  ;;

  let read comp =
    try
      ( Some comp.instructions.(comp.pointer)
      , Some comp.instructions.(comp.pointer + 1) )
    with
    | Failure _ | Invalid_argument _ -> None, None
  ;;
end

let parse lines =
  let read_register line =
    String.split line ~on:' ' |> List.last_exn |> Int.of_string
  in
  let parts = Advent.split_on_empty_line lines in
  let registers = List.hd_exn parts in
  let a = List.nth_exn registers 0 |> read_register in
  let b = List.nth_exn registers 1 |> read_register in
  let c = List.nth_exn registers 2 |> read_register in
  let program =
    List.last_exn parts
    |> List.hd_exn
    |> String.split ~on:' '
    |> List.last_exn
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Array.of_list
  in
  Computer.{ a; b; c; instructions = program; pointer = 0; output = [] }
;;

let format_output (computer : Computer.t) =
  List.map computer.output ~f:Int.to_string |> String.concat ~sep:","
;;

let rec run (computer : Computer.t) =
  match Computer.read computer with
  | Some op, Some lit -> Computer.execute op lit computer |> run
  | _ -> computer
;;

let take_afteri after = Array.filteri ~f:(fun i _ -> i >= after)

let sequence_equal list array =
  Sequence.equal
    (fun a b -> a = b)
    (Sequence.of_list list)
    (Array.to_sequence array)
;;

let solve input = Advent.read_lines input |> parse |> run |> format_output

let solve2 input =
  let computer = Advent.read_lines input |> parse in
  let len = Array.length computer.instructions in
  let rec aux (computer : Computer.t) n a =
    let target = take_afteri (len - n) computer.instructions in
    let computer = Computer.{ computer with a } in
    match run computer with
    | c when sequence_equal c.output target && n = len -> a
    | c when sequence_equal c.output target -> aux computer (n + 1) (a lsl 3)
    | _ -> aux computer n (a + 1)
  in
  aux computer 1 0
;;

let () = solve "inputs/day17.txt" |> Fmt.pr "\nPart 1: %s"
let () = solve2 "inputs/day17.txt" |> Fmt.pr "\nPart 2: %d"
