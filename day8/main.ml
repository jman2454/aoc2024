open Aoc

module IntTuple = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match Int.compare x1 x2 with 
    | 0 -> Int.compare y1 y2 
    | n -> n
end

module CharMap = Map.Make(Char)
module TupleSet = Set.Make(IntTuple)

let organize_points grid = 
  Grid.flipped_mapi (fun pos el -> 
    (pos, el)
  ) grid 
  |> Grid.fold_left (fun map (pos, el) -> 
    if el = '.' then map else 
    CharMap.update el (fun lst_opt -> 
      match lst_opt with 
      | None -> Some([pos])
      | Some(lst) -> Some(pos::lst)
    ) map
  ) CharMap.empty

let antinodes (x1, y1) (x2, y2) = 
  [
    (x1 + (x1 - x2), y1 + (y1 - y2));
    (x2 + (x2 - x1), y2 + (y2 - y1))
  ]

let combinations lst = 
  let rec h l acc = 
    match l with 
    | [] -> acc
    | el::rest -> 
      h rest (List.fold_left 
      (fun acc el2 -> 
        List.fold_left (fun set el -> TupleSet.add el set) acc (antinodes el el2)
      ) acc rest)
  in
  h lst TupleSet.empty

let answer input = 
  let grid = 
    input
    |> Grid.of_string
  in
  let map = 
    grid
    |> organize_points
    |> CharMap.map (fun el -> combinations el)
  in
  CharMap.fold (fun _ s1 s2 -> TupleSet.union s1 s2) map TupleSet.empty 
  |> TupleSet.elements
  |> List.filter (fun pos -> Grid.in_bounds pos grid)
  |> List.length

let () = 
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"
|> answer
|> string_of_int
|> print_endline