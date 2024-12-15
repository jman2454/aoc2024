let happy l = 
  List.mapi (fun i x -> (i, x)) l
  |> 
  List.fold_left (fun (acc, prev, dir, first_bad) (i, x) -> 
    if acc then 
      let good, dir = 
        match prev with 
        | None -> true, `Start
        | Some(prev) -> 
          let d = if x > prev && dir <> `Down then `Up else if x < prev && dir <> `Up then `Down else `None in 
          (
           (* ( if p then( print_endline @@ string_of_int i;
          print_endline @@ string_of_bool (d <> `None);) else ()); *)
          d <> `None && abs (prev - x) <= 3, d)
      in
      (good, Some(x), dir, if not good then i else first_bad)
    else
      (false, None, `None, first_bad)) (true, None, `Start, -1)
  |> fun (acc, _, _, first_bad) -> acc, first_bad

let omit i l = 
  List.filteri (fun j _ -> i <> j) l

let string_of_list str_of_el l = 
  if l = [] then "[]" else
  "[" ^ List.fold_left (fun acc x -> acc ^ ", " ^ str_of_el x) (List.hd l |> str_of_el) (List.tl l) ^ "]"

let happy_with_skip l = 
  let acc, first_bad = happy l in
  if acc then true else 
    fst @@ happy (omit first_bad l)
    || first_bad > 0 && fst (happy (omit (first_bad - 1) l))
    || first_bad > 1 && fst (happy (omit (first_bad - 2) l))

let answer lists = List.fold_left (fun acc l -> if happy_with_skip l then acc + 1 else acc) 0 lists

let parse_input s = 
  s |> String.split_on_char '\n' 
    |> List.map (fun s -> String.split_on_char ' ' s |> List.filter (fun s -> s <> "") |> List.map int_of_string)

let () = 
  parse_input "3 2 3 4"
  |> answer |> string_of_int |> print_endline