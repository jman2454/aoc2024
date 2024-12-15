open Aoc

let parse_rule rule = 
  match String.split_on_char '|' rule with 
  | [bottom; top] -> Some(int_of_string bottom, int_of_string top)
  | _ -> None

module IntMap = Map.Make(Int)

let build_map rules = 
  List.fold_left (fun map (bot, top) -> 
    IntMap.update bot (fun lst_opt -> Some(Option.fold ~none:[top] ~some:(fun l -> top::l) lst_opt)) map
  ) IntMap.empty rules

module IntSet = Set.Make(Int)

let reject_line lst map =
  List.fold_left (fun (seen, reject) n -> 
    if not reject then 
      match IntMap.find_opt n map with 
      | None -> (IntSet.add n seen, reject)
      | Some(lst) -> (IntSet.add n seen, any (fun m -> IntSet.mem m seen) lst)
    else
      (seen, true)
  ) (IntSet.empty, false) lst
  |> snd

let accept_line map line = not @@ reject_line line map

let answer rule_map lines = 
  List.filter (accept_line rule_map) lines
  |> List.map (fun lst -> List.nth lst (List.length lst / 2))
  |> List.fold_left (+) 0

let answer input = 
  String.split_on_char '\n' input
  |> List.fold_left (fun (rules, lines) line -> 
    match parse_rule line with 
    | None -> 
      let split = String.split_on_char ',' line in 
      if split = [] || split = [""] then 
        (rules, lines)
      else
        (rules, (List.map int_of_string split)::lines)
    | Some(rule) -> rule::rules, lines
  ) ([], [])
  |> fun (rules, lines) -> answer (build_map rules) lines
  
let () = 
answer "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47" 
|> string_of_int
|> print_endline