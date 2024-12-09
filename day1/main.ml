let answer l1 l2 = 
  List.combine (List.sort compare l1) (List.sort compare l2)
  |> List.fold_left (fun acc (x, y) -> acc + abs (x - y)) 0

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