open Aoc

let make_grid s = 
  String.split_on_char '\n' s
  |> Pvector.of_list
  |> Pvector.map (Pvector.of_string)

let read_pos (x, y) grid = (grid --> y) --> x

let pos_good (x, y) grid = 
  y < Pvector.len grid && y >= 0
  && x >= 0 && x < Pvector.len @@ grid --> y

let move (x, y) = function 
| `Left -> (x-1, y)
| `LeftUp -> (x-1, y-1)
| `LeftDown -> (x-1, y+1)
| `Right -> (x+1, y)
| `RightUp -> (x+1, y-1)
| `RightDown -> (x+1, y+1)
| `Up -> (x, y-1)
| `Down -> (x, y+1)

let rec find_appearance str stri grid pos dir acc = 
  if not @@ pos_good pos grid then acc else
  if read_pos pos grid = String.get str stri then 
    if stri = String.length str - 1 then acc + 1 else
      find_appearance str (stri + 1) grid (move pos dir) dir acc
  else
    acc

let all_in_direction str grid pos acc =
  List.fold_left (fun acc dir -> find_appearance str 0 grid pos dir acc) acc [
    `Left; `LeftUp; `LeftDown; `Right; `RightUp; `RightDown; `Up; `Down
  ]

let pos_of_i i row_len = 
  (i mod row_len, i / row_len)

let find_appearances str grid = 
  let row_len = Pvector.len (grid --> 0) in 
  let total_len = Pvector.len grid * row_len in 
  let rec h i acc = 
    if i = total_len then acc
    else h (i + 1) (all_in_direction str grid (pos_of_i i row_len) acc)
  in
  h 0 0

let () = 
find_appearances "XMAS" (make_grid "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")
|> string_of_int
|> print_endline