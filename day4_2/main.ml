open Aoc

let make_grid s = 
  String.split_on_char '\n' s
  |> Pvector.of_list
  |> Pvector.map Pvector.of_string

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

let rec find_appearance str stri grid pos dir = 
  if not @@ pos_good pos grid then false else
  if read_pos pos grid = String.get str stri then 
    if stri = String.length str - 1 then true else
      find_appearance str (stri + 1) grid (move pos dir) dir
  else
    false

let all_a_positions str grid pos =
  List.filter (fun dir -> find_appearance str 0 grid pos dir) [
    `Left; `LeftUp; `LeftDown; `Right; `RightUp; `RightDown; `Up; `Down
  ]
  |> List.map (fun dir -> pos, move pos dir)

let pos_of_i i row_len = 
  (i mod row_len, i / row_len)

let i_of_pos (x, y) row_len = x + y * row_len

(* returns all of the (p1, p2) where p1 is an M, p2 is an A, and there's a p3 that's an s *)
(* st p1p2p3 form a "straight contiguous line" in the grid *)
let find_appearances str grid = 
  let row_len = Pvector.len (grid --> 0) in 
  let total_len = Pvector.len grid * row_len in 
  let rec h i acc = 
    if i = total_len then acc
    else 
      h (i + 1) (
        acc 
        @ (all_a_positions str grid 
            (pos_of_i i row_len) 
            |> List.map (fun (m_pos, a_pos) -> (m_pos, i_of_pos a_pos row_len)))
      )
  in
  h 0 []

module IntMap = Map.Make(Int)

let count_close lst = 
  let close (x1, y1) (x2, y2) = abs (x1 - x2) = 2 && y1 = y2 || abs (y1 - y2) = 2 && x1 = x2 in
  let rec h l acc = 
    match l with 
    | [] -> acc
    | p1::rest -> 
      h rest (acc + if any (fun p2 -> close p1 p2) rest then 1 else 0)
  in
  h lst 0

let count_appearances values  = 
 let map = List.fold_left (fun map (m_pos, a_pos) -> 
    IntMap.update a_pos (fun v_opt -> Some(Option.fold ~none:[m_pos] ~some:(fun v -> m_pos::v) v_opt)) map
  ) IntMap.empty values
 in
  IntMap.fold (fun _ value acc -> value::acc) map []
  |> List.fold_left (fun acc lst -> acc + count_close lst) 0

let answer s = 
  make_grid s 
    |> find_appearances "MAS"
    |> count_appearances

    
let () = 
answer ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
.........."
|> string_of_int
|> print_endline