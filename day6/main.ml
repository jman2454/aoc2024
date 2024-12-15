open Aoc

type direction = Up | Down | Left | Right

let move (x, y) = function 
| Up -> (x, y - 1)
| Down -> (x, y + 1)
| Left -> (x - 1, y)
| Right -> (x + 1, y)

let invert = function | Up -> Down | Down -> Up | Left -> Right | Right -> Left
let rotate_right = function | Up -> Right | Right -> Down | Down -> Left | Left -> Up

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let rec find c start dir grid = 
  if not @@ Grid.in_bounds start grid then 
    None
  else if Grid.at start grid = c then 
    Some(start)
  else
    find c (move start dir) dir grid

let rec boundary pos dir grid = 
  if not @@ Grid.in_bounds pos grid then (move pos (invert dir)) else
  boundary (move pos dir) dir grid

let rec count_path pos dir grid acc = 
  match find '#' pos dir grid with 
  | None -> acc + (distance pos @@ boundary pos dir grid)
  | Some(obstacle_pos) -> 
    let next_pos = move obstacle_pos (invert dir) in 
    count_path next_pos (rotate_right dir) grid (acc + distance pos next_pos)

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
  count_path (start_pos grid) Up grid 0

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
