(* in the actual input, longest sequence is 10 numbers, so 9 operators *)
(* since options are + and *, worst number of possibilities to try is 2^9 per line *)

let test result numbers = 
  let rec h lst acc = 
    match lst with 
    | [] -> acc = result
    | a::rest -> h rest (acc * a) || h rest (acc + a)
  in
  match numbers with 
  | [] -> false
  | a::[] -> a = result
  | a::rest -> h rest a

let parse_input s = 
  String.split_on_char '\n' s 
  |> List.map (fun line -> 
    match String.split_on_char ':' line with 
    | [result; numbers] -> 
      int_of_string result, 
      String.split_on_char ' ' (String.trim numbers)
      |> List.map int_of_string
    | _ -> failwith "invalid input!"
  )

let answer s = 
  parse_input s 
  |> List.filter (fun (result, numbers) -> test result numbers)
  |> List.map (fun (r, _) -> r)
  |> List.fold_left (+) 0

let () = 
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"
|> answer
|> string_of_int
|> print_endline