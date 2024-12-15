let happy l = 
  List.fold_left (fun (acc, prev, dir) x -> 
    if acc then 
      let good = 
        match prev with 
        | None -> true
        | Some(prev) -> 
          let d = if x > prev && dir <> `Down then `Up else if x < prev && dir <> `Up then `Down else `None in 
          d <> `None && abs (prev - x) <= 3
      in
      (good, Some(x), dir)
    else
      (false, None, `None)) (true, None, `Start) l
  |> fun (acc, _, _) -> acc

let answer lists = List.fold_left (fun acc l -> if happy l then acc + 1 else acc) 0 lists

let parse_input s = 
  s |> String.split_on_char '\n' 
    |> List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> s <> "") |> List.map int_of_string)

let () = 
  parse_input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9" 
  |> answer |> string_of_int |> print_endline