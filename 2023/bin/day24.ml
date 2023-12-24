open Core
open Advent.Parser

type hail =
  { x : float
  ; y : float
  ; z : float
  ; vx : float
  ; vy : float
  ; vz : float
  }
[@@deriving show]

let parse_hail =
  let* x = fdigit <* string "," <* whitespace in
  let* y = fdigit <* string "," <* whitespace in
  let* z = fdigit <* whitespace <* string "@" <* whitespace in
  let* vx = fdigit <* string "," <* whitespace in
  let* vy = fdigit <* string "," <* whitespace in
  let* vz = fdigit in
  return { x; y; z; vx; vy; vz }
;;

let parse_input = sep_by1 newline parse_hail

let linear_equation hail =
  let x2 = Float.(hail.x + hail.vx) in
  let y2 = Float.(hail.y + hail.vy) in
  let a = y2 -. hail.y in
  let b = hail.x -. x2 in
  let c = (hail.y *. (x2 -. hail.x)) -. ((y2 -. hail.y) *. hail.x) in
  a, b, c
;;

let poi (a, b, c) (a', b', c') =
  let xo = ((b *. c') -. (b' *. c)) /. ((a *. b') -. (a' *. b)) in
  let yo = ((c *. a') -. (c' *. a)) /. ((a *. b') -. (a' *. b)) in
  if Float.(xo = infinity) || Float.(yo = infinity) then None else Some (xo, yo)
;;

let in_future hail (xo, _) =
  let next = Float.(hail.x + hail.vx) in
  if Float.(next < hail.x) then Float.(xo < hail.x) else Float.(xo > hail.x)
;;

let in_bounds (xo, yo) =
  let min = 200000000000000. in
  let max = 400000000000000. in
  Float.( <= ) min xo
  && Float.( <= ) xo max
  && Float.( <= ) min yo
  && Float.( <= ) yo max
;;

let calc hail hail' =
  let l1 = linear_equation hail in
  let l2 = linear_equation hail' in
  let intersection = poi l1 l2 in
  match intersection with
  | None -> false
  | Some intersection ->
    in_future hail intersection
    && in_future hail' intersection
    && in_bounds intersection
;;

let part1 hails =
  let rec aux hails acc =
    match hails with
    | [] -> acc
    | [ _ ] -> acc
    | h :: rest ->
      let acc =
        List.fold rest ~init:acc ~f:(fun acc hail' ->
          if calc h hail' then acc + 1 else acc)
      in
      aux rest acc
  in
  aux hails 0 |> sprintf "Part 1: %d" |> print_endline
;;

let () =
  let line = Advent.read_all "./inputs/day24-test.txt" in
  let hails =
    match parse_string ~consume:Prefix parse_input line with
    | Ok hails -> hails
    | Error e -> failwith e
  in
  let _ = part1 hails in
  ()
;;

(*
   solved on live.sympy.org

   import sympy
   init_printing()

   # lines = ["----","----"]

   hailstones = [tuple(map(int, line.replace("@", ",").replace(" ", "").split(","))) for line in lines]

   xr, yr, zr, vxr, vyr, vzr = sympy.symbols("xr, yr, zr, vxr, vyr, vzr")

   equations = []

   for i, (sx, sy, sz, vx, vy, vz) in enumerate(hailstones):
   equations.append((xr - sx) * (vy - vyr) - (yr - sy) * (vx - vxr))
   equations.append((yr - sy) * (vz - vzr) - (zr - sz) * (vy - vyr))
   if i < 2:
   continue
   answers = [soln for soln in sympy.solve(equations) if all(x % 1 == 0 for x in soln.values())]
   if len(answers) == 1:
   break

   answer = answers[0]

   print(answer[xr] + answer[yr] + answer[zr])
   print(i)
*)
