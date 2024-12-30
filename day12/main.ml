open Aoc
module CharMap = Map.Make(Char)

(* 
  encoding of corner spots (as tuples)
  and naively adding the appropriate corners for each perimeter block

  row_count+1 rows of length col_count+1

  in the below, x is row and y is col
  block (0,0) has corners (0,0), (0,1), (1,0) and (1,1)

  block (1,0) has corners (1,0), (1,1), (2,0) and (2,1)

  etc...
*)

module IntTuple = 
struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = 
    match Int.compare x1 x2 with 
    | 0 -> Int.compare y1 y2 
    | n -> n
end

module TupleSet = Set.Make(IntTuple)
module TupleMap = Map.Make(IntTuple)

let same_value c n_pos grid = Grid.in_bounds n_pos grid && Grid.at n_pos grid = c
let neighbors (x,y) = [(x+1,y);(x,y+1);(x-1,y);(x,y-1)]

let merge_regions grid = 
  let uf = GridUnionFind.of_data grid in 
  Grid.fold_lefti (fun uf pos el -> 
    List.fold_left (fun uf neighb -> 
      if not @@ Grid.in_bounds neighb grid then
        uf
      else (match GridUnionFind.same pos neighb uf with
      (* if not in the same region but neighbors share value, merge them *)
      | (false, uf) when same_value el neighb grid -> GridUnionFind.union pos neighb uf
      | (_, uf) -> uf)
    ) uf (neighbors pos)
  ) uf grid
  |> GridUnionFind.compress

let perim_and_area uf = 
  let maybe_add cond value corners = if cond then TupleSet.add value corners else corners in 
  GridUnionFind.fold (fun (x,y) _ corners -> 
    let set_id = GridUnionFind.find_no_compress (x,y) uf in
    let set, area = if TupleMap.mem set_id corners then TupleMap.find set_id corners else TupleSet.empty, 0 in 

    (* direction checks are O(1) *)
    let left = GridUnionFind.same_no_compress (x,y) (x,y-1) uf in 
    let right = GridUnionFind.same_no_compress (x,y) (x,y+1) uf in 
    let top = GridUnionFind.same_no_compress (x,y) (x-1,y) uf in 
    let bottom = GridUnionFind.same_no_compress (x,y) (x+1,y) uf in 

    (* check up corners -- rare case in which we need to count a corner position twice for the same region! *)
    let up_left = GridUnionFind.same_no_compress (x,y) (x-1,y-1) uf in 
    let up_right = GridUnionFind.same_no_compress (x,y) (x-1,y+1) uf in 

    let set = 
      set
      |> maybe_add (not left || not top) (x,y)
      |> maybe_add (not left || not bottom) (x+1,y)
      |> maybe_add (not right || not top) (x,y+1)
      |> maybe_add (not right || not bottom) (x+1,y+1)

      |> maybe_add (up_left && (not top && not left)) (-x,-y)
      |> maybe_add (up_right && (not top && not right)) (-x,-(y+1))
    in
    TupleMap.add set_id (set, area + 1) corners
  ) TupleMap.empty uf
  (* check both up diags for corners in same shape -- need to add 1 for those *)
  |> TupleMap.map (fun (corners, area) -> (TupleSet.cardinal corners, area))

let answer input = 
  (Grid.of_string input 
  |> merge_regions
  |> perim_and_area
  |> TupleMap.fold (fun _ (perim, area) acc -> acc + perim * area)) 0

let () = 
"TTT
TQT
TTQ"
|> answer
|> string_of_int
|> print_endline