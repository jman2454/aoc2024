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