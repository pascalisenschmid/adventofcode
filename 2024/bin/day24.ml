open Core

type op =
  | OR
  | AND
  | XOR
[@@deriving equal]

let parse_value line =
  match
    String.substr_replace_all line ~pattern:" " ~with_:""
    |> String.split ~on:':'
  with
  | [ name; value ] -> name, Int.of_string value
  | _ -> failwith "invalid value definition"
;;

let parse_instruction line =
  match String.split ~on:' ' line with
  | [ v1; "XOR"; v2; _; res ] -> v1, XOR, v2, res
  | [ v1; "AND"; v2; _; res ] -> v1, AND, v2, res
  | [ v1; "OR"; v2; _; res ] -> v1, OR, v2, res
  | _ -> failwith "invalid instruction"
;;

let parse lines =
  match Advent.split_on_empty_line lines with
  | [ first; last ] ->
    ( List.map first ~f:parse_value
      |> List.fold
           ~init:(Hashtbl.create (module String))
           ~f:(fun acc (name, value) ->
             Hashtbl.add_exn acc ~key:name ~data:value;
             acc)
    , List.map last ~f:parse_instruction )
  | _ -> failwith "invalid input"
;;

let queue_of_list list =
  let queue = Queue.create () in
  Queue.enqueue_all queue list;
  queue
;;

let evaluate values instructions =
  let rec aux queue map =
    match Queue.dequeue queue with
    | None -> map
    | Some (op1, op, op2, res) ->
      (match Hashtbl.find map op1, Hashtbl.find map op2 with
       | Some v1, Some v2 ->
         let data =
           match op with
           | OR -> Int.bit_or v1 v2
           | AND -> Int.bit_and v1 v2
           | XOR -> Int.bit_xor v1 v2
         in
         Hashtbl.add_exn map ~key:res ~data;
         aux queue map
       | _ ->
         Queue.enqueue queue (op1, op, op2, res);
         aux queue map)
  in
  aux instructions values
;;

let extract_value values ~prefix =
  Hashtbl.to_alist values
  |> List.filter ~f:(fun (k, _) -> String.is_prefix k ~prefix)
  |> List.sort ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2)
  |> List.foldi ~init:0 ~f:(fun idx acc (_, v) -> Int.bit_or acc (v lsl idx))
;;

let find_wrong_connections instructions =
  let is_xyz a = Char.equal a 'x' || Char.equal a 'y' || Char.equal a 'z' in
  let is_x00 a = String.equal a "x00" in
  List.fold
    instructions
    ~init:(Set.empty (module String))
    ~f:(fun acc (op1, op, op2, res) ->
      let op1_type = String.to_list op1 |> List.hd_exn in
      let op2_type = String.to_list op2 |> List.hd_exn in
      let res_type = String.to_list res |> List.hd_exn in
      match op, op1_type, op2_type, res_type with
      | AND, _, _, 'z' when not (String.equal res "z45") -> Set.add acc res
      | OR, _, _, 'z' when not (String.equal res "z45") -> Set.add acc res
      | XOR, s1, s2, sr
        when (not (is_xyz s1)) && (not (is_xyz s2)) && not (is_xyz sr) ->
        Set.add acc res
      | AND, _, _, _ when (not (is_x00 op1)) && not (is_x00 op2) ->
        List.fold instructions ~init:acc ~f:(fun acc (op1', op', op2', _) ->
          if
            (String.equal res op1' || String.equal res op2')
            && not (equal_op op' OR)
          then Set.add acc res
          else acc)
      | XOR, _, _, _ ->
        List.fold instructions ~init:acc ~f:(fun acc (op1', op', op2', _) ->
          if (String.equal res op1' || String.equal res op2') && equal_op op' OR
          then Set.add acc res
          else acc)
      | _ -> acc)
;;

let () =
  let values, instructions = Advent.read_lines "inputs/day24.txt" |> parse in
  let instructions = queue_of_list instructions in
  let values = evaluate values instructions in
  extract_value values ~prefix:"z" |> Fmt.pr "Part 1: %d"
;;

let () =
  let _, instructions = Advent.read_lines "inputs/day24.txt" |> parse in
  find_wrong_connections instructions
  |> Set.to_list
  |> List.sort ~compare:String.compare
  |> String.concat ~sep:","
  |> Fmt.pr "\nPart 2: %s"
;;
