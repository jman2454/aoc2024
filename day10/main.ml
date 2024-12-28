open Aoc

module TupleSet = 
Set.Make(
  struct
    type t = int * int
    let compare (x1, y1) (x2, y2) = 
      match Int.compare x1 x2 with 
      | 0 -> Int.compare y1 y2 
      | n -> n
  end
)

let neighbors (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
let max = 9

let reachable_nines start grid =
  let rec h start visited nines = 
    if not @@ (Grid.in_bounds start grid) || TupleSet.mem start visited then nines else
    let n = Grid.at start grid in 
    if n = max then TupleSet.add start nines else
    let visited = TupleSet.add start visited in
    List.fold_left (fun nines neighbor -> 
      match Grid.at_opt neighbor grid with 
      | Some(m) when m = n + 1 -> h neighbor visited nines
      | _ -> nines
    ) nines (neighbors start)
  in
  h start TupleSet.empty TupleSet.empty

let zeros grid = 
  Grid.mapi (fun pos el -> if el = 0 then Some(pos) else None) grid 
  |> Grid.fold_left (fun lst opt -> Option.fold ~none:lst ~some:(fun pos -> pos::lst) opt) []

let char_to_int c = String.make 1 c |> (fun c -> if c = "." then -1 else int_of_string c)

let answer input = 
  let grid = input |> Grid.of_string |> Grid.mapi (fun _ el -> char_to_int el) in
  List.fold_left (fun acc start -> acc + (TupleSet.cardinal @@ reachable_nines start grid)) 0 (zeros grid)

let () = 
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"
|> answer
|> string_of_int
|> print_endline