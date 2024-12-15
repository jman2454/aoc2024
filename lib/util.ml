(* fn composition *)
let (>>) g f = fun x -> f (g x)

let parse_int_list s = 
  String.split_on_char ' ' s
  |> List.filter ((=) ' ' |> String.for_all >> not)
  |> List.map (fun s -> String.trim s |> int_of_string)

let string_of_list str_of_el l = 
  if l = [] then "[]" else
  "[" ^ List.fold_left (fun acc x -> acc ^ ", " ^ str_of_el x) (List.hd l |> str_of_el) (List.tl l) ^ "]"

let print_return_int i = 
  Printf.printf "%d\n" i; i

let any pred lst = List.fold_left (fun found nxt -> found || pred nxt) false lst