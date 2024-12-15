(* get a comparator for els based on the ranking and stable sort the lines *)
open Aoc

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let has_incoming_edges k map = 
  IntMap.mem k map && not @@ IntSet.is_empty (IntMap.find k map)

(* only need to update the incoming map -- we only process the outgoing map on each node once *)
let rec topo q l outgoing incoming = 
  if AocQueue.is_empty q then 
    if not @@ IntMap.is_empty incoming then 
      failwith "cycles!"
    else
      l |> List.rev
  else
    let n = AocQueue.front q in 
    let q = AocQueue.deq q in 
    match IntMap.find_opt n outgoing with
    | None -> topo q (n::l) outgoing incoming
    | Some(s) when IntSet.is_empty s -> topo q (n::l) outgoing incoming
    | Some(out_edges) -> 
      let q, incoming = IntSet.fold (fun m (q, incoming) -> 
        let incoming = 
          IntMap.update m (fun in_opt -> 
            match in_opt with 
            | None -> None
            | Some(in_edges) -> 
              let in_edges = IntSet.remove n in_edges in 
              if IntSet.is_empty in_edges then None else Some(in_edges)
          ) incoming
        in 
        if has_incoming_edges m incoming then
          (q, incoming)
        else
          (AocQueue.enq m q, incoming)
      ) out_edges (q, incoming) in 
      topo q (n::l) outgoing incoming

let parse_rule rule = 
  match String.split_on_char '|' rule with 
  | [bottom; top] -> Some(int_of_string bottom, int_of_string top)
  | _ -> None

let build_relation lines = 
  lines
  |> List.map (fun line -> parse_rule line)
  |> List.filter Option.is_some
  |> List.map Option.get

let parse_seqs lines = 
  lines
  |> List.filter (fun s -> String.contains s ',')
  |> List.map (fun line -> String.split_on_char ',' line |> List.map int_of_string)
  
let parse_input input = 
  let lines = String.split_on_char '\n' input in 
  build_relation lines, parse_seqs lines

let build_graph relation nodes_of_interest = 
  relation
  |> List.fold_left (fun (outgoing, incoming, nodes) (a, b) -> 
    if not (IntSet.mem a nodes_of_interest) || not (IntSet.mem b nodes_of_interest) then 
      (outgoing, incoming, nodes) 
    else
      IntMap.update a (function 
        | None -> Some(IntSet.empty |> IntSet.add b)
        | Some(set) -> Some(set |> IntSet.add b)) outgoing,
        IntMap.update b (function 
        | None -> Some(IntSet.empty |> IntSet.add a)
        | Some(set) -> Some(set |> IntSet.add a)) incoming,
        nodes |> IntSet.add a |> IntSet.add b
  ) (IntMap.empty, IntMap.empty, IntSet.empty)

let rank list = 
  List.mapi (fun i el -> (i, el)) list
  |> List.fold_left (fun map (i, el) -> IntMap.add el i map) IntMap.empty

let topo seq relation = 
  let (outgoing, incoming, nodes) = build_graph relation (IntSet.of_list seq) in 
  let no_incoming = 
    IntSet.filter (fun n -> not @@ IntMap.mem n incoming) nodes 
  in 
  let no_incoming = IntSet.fold (fun n q -> AocQueue.enq n q) no_incoming AocQueue.empty in
  topo no_incoming [] outgoing incoming
  |> rank

let sort_seq seq relation = 
  let rankings = topo seq relation in 
  List.sort (fun a b -> Int.compare (IntMap.find a rankings) (IntMap.find b rankings)) seq

let middle_if_sorted relation seq = 
  let sorted = sort_seq seq relation in 
  if List.equal (fun a b -> a = b) sorted seq then 
    0
  else
    List.nth sorted (List.length sorted / 2)

let answer input = 
  let relation, seqs = parse_input input in 
  List.map (middle_if_sorted relation) seqs
  |> List.fold_left (+) 0

let () = 
"47|53
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
|> answer
|> string_of_int 
|> print_endline