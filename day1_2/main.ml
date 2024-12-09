include Map

module IntMap = Map.Make(Int)

let counts l1 = 
  List.fold_left (fun acc x -> 
    IntMap.update x (fun v -> Some(if Option.is_none v then 1 else Option.get v + 1)) acc) IntMap.empty l1

let answer l1 l2 = 
  let counts = counts l2 in
  List.fold_left (fun acc x ->
    match IntMap.find_opt x counts with
      | Some(v) -> v * x + acc
      | None -> acc) 0 l1

let parse_input s = 
  s |> String.split_on_char '\n' 
    |> List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> s <> "") |> List.map int_of_string)
    |> List.map (fun l -> List.hd l, List.hd (List.tl l))
    |> List.split

let () = 
  let l1, l2 = parse_input "3   4
4   3
2   5
1   3
3   9
3   3" in 
  answer l1 l2 |> string_of_int |> print_endline