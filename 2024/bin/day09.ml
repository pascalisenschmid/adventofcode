open Core

type file =
  { start : int
  ; length : int
  }

type block =
  | Free
  | File

let make_array line =
  let rec aux acc id block line =
    match line, block with
    | hd :: rest, File ->
      aux (Array.append acc (Array.create ~len:hd id)) (id + 1) Free rest
    | hd :: rest, Free ->
      aux (Array.append acc (Array.create ~len:hd (-1))) id File rest
    | [], _ -> acc
  in
  aux [||] 0 File (String.to_list line |> List.map ~f:Char.get_digit_exn)
;;

let checksum arr =
  arr
  |> Array.mapi ~f:(fun i n ->
    match n with
    | -1 -> 0
    | n -> i * n)
  |> Array.to_list
  |> List.fold ~init:0 ~f:Int.( + )
;;

let find_file max arr idx =
  let cursor = ref (idx - 1) in
  while !cursor >= 0 && arr.(!cursor) = -1 do
    cursor := !cursor - 1
  done;
  if !cursor < 0
  then None
  else (
    let _end = !cursor in
    while !cursor >= 0 && arr.(!cursor) = arr.(_end) && _end - !cursor < max do
      cursor := !cursor - 1
    done;
    Some { start = !cursor + 1; length = _end - !cursor })
;;

let rec is_free arr i size =
  if size = 0
  then true
  else if i >= Array.length arr || arr.(i) <> -1
  then false
  else is_free arr (i + 1) (size - 1)
;;

let find_free_space arr size : int option =
  let i = ref 0 in
  while !i < Array.length arr && not (is_free arr !i size) do
    i := !i + 1
  done;
  if !i = Array.length arr then None else Some !i
;;

let move arr f i =
  for k = 0 to f.length - 1 do
    arr.(i + k) <- arr.(f.start + k);
    arr.(f.start + k) <- -1
  done
;;

let defragment max arr : unit =
  let rec aux arr len =
    match find_file max arr len with
    | None -> ()
    | Some f ->
      (match find_free_space arr f.length with
       | None -> aux arr f.start
       | Some i ->
         if i < f.start then move arr f i;
         aux arr f.start;
         ())
  in
  aux arr (Array.length arr)
;;

let () =
  let line = Advent.read_lines "inputs/day09.txt" |> List.hd_exn in
  let nums1 = make_array line in
  defragment 1 nums1;
  checksum nums1 |> Fmt.pr "\nPart 1: %d";
  let nums2 = make_array line in
  defragment 10 nums2;
  checksum nums2 |> Fmt.pr "\nPart 2: %d";
  ()
;;
