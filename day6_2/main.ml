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
  let compare (x1, y1) (x2, y2) = 
    match Int.compare x1 x2 with 
    | 0 -> Int.compare y1 y2 
    | n -> n
end

module DirectedIntTuple = struct
  type t = (int * int) * direction
  let compare ((x1, y1), d1) ((x2, y2), d2) = 
    let d_to_num = function | Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3 in 
    match Int.compare x1 x2 with 
    | 0 -> 
      (match Int.compare y1 y2 with 
      | 0 -> Int.compare (d_to_num d1) (d_to_num d2)
      | n -> n)
    | n -> n
end

module TupleSet = Set.Make(IntTuple)
module DTupleSet = Set.Make(DirectedIntTuple)

type state = { pos : int * int; dir : direction; grid : char Grid.t }

let rec loops state visited = 
  if not @@ Grid.in_bounds state.pos state.grid then 
    false
  else if Grid.at state.pos state.grid = '#' then 
    loops { state with pos = (move state.pos (invert state.dir)); dir = rotate_right state.dir } visited
  else if DTupleSet.mem (state.pos, state.dir) visited then 
    true
  else
    loops { state with pos = move state.pos state.dir } (DTupleSet.add (state.pos, state.dir) visited)

let rec walk state loop_starters visited = 
  if not @@ Grid.in_bounds state.pos state.grid then 
    List.length @@ TupleSet.elements loop_starters
  else if Grid.at state.pos state.grid = '#' then
    walk { state with pos = (move state.pos (invert state.dir)); dir = rotate_right state.dir } loop_starters visited
  else
    let loop_starters = 
      let obs_pos = move state.pos state.dir in 
      if Grid.in_bounds obs_pos state.grid     (* can only place in bounds *)
        && Grid.at obs_pos state.grid <> '^'   (* can't place at start position *)
        && not @@ TupleSet.mem obs_pos visited (* can't place where we would have already walked *)
        && loops { 
          state with 
          dir = rotate_right state.dir; 
          grid = Grid.set obs_pos '#' state.grid
        } DTupleSet.empty then 
        TupleSet.add obs_pos loop_starters 
      else
        loop_starters
    in
    walk { state with pos = move state.pos state.dir } loop_starters (TupleSet.add state.pos visited)

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
  walk { pos = (start_pos grid); dir = Up; grid = grid } TupleSet.empty TupleSet.empty

let () = 
"..........
.#........
..........
...#......
.........#
#.#.#....#
....^....#
....#.....
..........
........#."
|> Grid.of_string
|> answer
|> string_of_int
|> print_endline