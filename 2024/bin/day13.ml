open Core

type machine =
  { ax : float
  ; ay : float
  ; bx : float
  ; by : float
  ; px : float
  ; py : float
  }
[@@deriving show]

let parse_line splitchars line =
  let split = String.split_on_chars line ~on:splitchars in
  ( List.nth_exn split 1 |> Float.of_string
  , List.nth_exn split 3 |> Float.of_string )
;;

let solve m offset =
  let m =
    { ax = m.ax
    ; ay = m.ay
    ; bx = m.bx
    ; by = m.by
    ; px = m.px +. offset
    ; py = m.py +. offset
    }
  in
  let b =
    ((m.ax *. m.py) -. (m.ay *. m.px)) /. ((m.ax *. m.by) -. (m.ay *. m.bx))
  in
  let a = (m.px -. (m.bx *. b)) /. m.ax in
  match b %. 1., a %. 1. with
  | 0., 0. -> (a *. 3.) +. b |> Int.of_float
  | _ -> 0
;;

let () =
  let _ =
    let machines =
      Advent.read_lines "inputs/day13.txt"
      |> Advent.split_on_empty_line
      |> List.map ~f:(fun sublist ->
        let ax, ay = List.nth_exn sublist 0 |> parse_line [ '+'; ',' ] in
        let bx, by = List.nth_exn sublist 1 |> parse_line [ '+'; ',' ] in
        let px, py = List.nth_exn sublist 2 |> parse_line [ '='; ',' ] in
        { ax; ay; bx; by; px; py })
    in
    machines
    |> List.fold ~init:0 ~f:(fun acc m -> solve m 0. + acc)
    |> Fmt.pr "\nPart 1: %d";
    machines
    |> List.fold ~init:0 ~f:(fun acc m -> solve m 10000000000000. + acc)
    |> Fmt.pr "\nPart 2: %d"
  in
  ()
;;
