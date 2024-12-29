open Aoc
module CharMap = Map.Make(Char)

let add c opt = Some(Option.fold ~none:c ~some:((+) c) opt)
let inc opt = add 1 opt

(* basically create priority system for counting the 4 corners around a given spot *)

(* count top left if no one above, left, nor above-left-diag *)
(* count top right  *)

(* Ah, I think we need union find-like approach to account for disjoint regions which share the same character *)
(* esp consider things touching only at a diag point -- I think these would be separate regions *)

(* now we will update algorithm + matching neighbor *)
(* first, sweep and make compressed union find *)
(* then matching_neighbor becomes "is this neighbor in same unionfind set" *)
let matching_neighbor c n_pos grid = Grid.in_bounds n_pos grid && Grid.at n_pos grid = c

let perim_count (x,y) grid = 
  let c = Grid.at (x,y) grid in 
  let right = matching_neighbor c (x,y+1) grid in
  let below = matching_neighbor c (x+1,y) grid in 
  let diag = matching_neighbor c (x+1,y+1) grid in 
  1
  |> (+) (if not below then 1 else 0) 
  |> (+) (if not right then 1 else 0) 
  |> (+) (if not (right || below || diag) then 1 else 0)

let count_regions grid = 
  Grid.mapi (fun pos el -> (pos, el)) grid 
  |> Grid.fold_left (fun (p, a) (pos, c) -> 
    CharMap.update c (add @@ perim_count pos grid) p, CharMap.update c inc a
  ) (CharMap.empty, CharMap.empty)

let answer input = 
  (Grid.of_string input 
  |> count_regions
  |> (fun (a, p) -> 
    CharMap.merge (fun c area perim -> 
      print_endline @@ String.make 1 c ^ ": area=" ^ (string_of_int @@ Option.get area) ^ ", perim=" ^ (string_of_int @@ Option.get perim);
      Some(Option.get area * Option.get perim)) a p
  )
  |> CharMap.fold (fun _ value acc -> value + acc)) 0

let () = 
"AAAA
BBCD
BBCC
EEEC"
|> answer
|> string_of_int
|> print_endline