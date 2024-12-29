type 'a t = 'a Pvector.t Pvector.t

let of_string (s : string) : char t = 
  String.split_on_char '\n' s
  |> Pvector.of_list
  |> Pvector.map (Pvector.of_string)

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let at (x, y) (grid : 'a t) = (grid --> y) --> x

let in_bounds (x, y) (grid : 'a t) = 
  y < Pvector.len grid && y >= 0
  && x >= 0 && x < Pvector.len @@ grid --> y

let at_opt pos (grid : 'a t) = 
  if in_bounds pos grid then Some(at pos grid) else None

let set (x, y) value grid = 
  (grid, y) <-- ((grid --> y, x) <-- value)

let fold_left fn acc (grid : 'a t) = 
  Pvector.fold_left (fun acc row -> 
    Pvector.fold_left fn acc row
  ) acc grid

let fold_lefti fn acc (grid : 'a t) = 
  Pvector.fold_lefti (fun acc rowi row -> 
    Pvector.fold_lefti (fun acc coli value -> fn acc (coli, rowi) value) acc row
  ) acc grid

let mapi fn (grid : 'a t) = 
  Pvector.mapi (fun rowi row -> 
    Pvector.mapi (fun coli value -> fn (coli, rowi) value) row
  ) grid

let map fn (grid : 'a t) = 
  Pvector.map (fun row -> 
    Pvector.map (fun value -> fn value) row
  ) grid