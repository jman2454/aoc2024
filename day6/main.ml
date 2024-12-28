open Aoc

type direction = Up | Down | Left | Right

let move (x, y) = function 
| Up -> (x, y - 1)
| Down -> (x, y + 1)
| Left -> (x - 1, y)
| Right -> (x + 1, y)

let invert = function | Up -> Down | Down -> Up | Left -> Right | Right -> Left
let rotate_right = function | Up -> Right | Right -> Down | Down -> Left | Left -> Up

module IntTuple = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = match Int.compare x1 x2 with | 0 -> Int.compare y1 y2 | n -> n
end

module TupleSet = Set.Make(IntTuple)

(* exclusive of the found character's location, inclusive of all points traversed (even when we don't find c) *)
let rec points_to_char c start dir grid acc = 
  if not @@ Grid.in_bounds start grid then 
    acc, None
  else if Grid.at start grid = c then 
    acc, Some(start)
  else
    points_to_char c (move start dir) dir grid (TupleSet.add start acc)

let rec count_path pos dir grid acc = 
  match points_to_char '#' pos dir grid acc with 
  | set, None -> set
  | set, Some(obstacle_pos) -> 
    let next_pos = move obstacle_pos (invert dir) in 
    count_path next_pos (rotate_right dir) grid set

let start_pos grid = 
  let rec h pos = 
    if not (Grid.in_bounds pos grid) then failwith "not found!" else
    if Grid.at pos grid = '^' then pos else 
    let right = move pos Right in 
    if Grid.in_bounds right grid then 
      h right
    else
      let (_, y) = move pos Down in 
      h (0, y)
  in
  h (0, 0)

let answer grid = 
  count_path (start_pos grid) Up grid TupleSet.empty
  |> TupleSet.elements
  |> List.length

let () = 
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#..."
|> Grid.of_string
|> answer
|> string_of_int
|> print_endline
