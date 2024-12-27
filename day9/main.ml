open Aoc

type block = 
| Free of { len : int }
| Taken of { len : int; id : int }

let char_to_int c = 
  String.make 1 c |> int_of_string

let parse input = 
  let rec h i acc = 
    if i >= String.length input then 
      acc
    else
      h (i + 1) (
        Pvector.append 
        (
          let len = char_to_int (String.get input i) in 
          if i mod 2 = 0 then Taken({ len = len; id = i / 2 }) else Free({ len = len })
        )
        acc
      )
  in
  h 0 Pvector.empty

let fold_n fn ct acc = 
  let rec h acc i = 
    if i = ct then acc else 
    h (fn i acc) (i + 1)
  in
  h acc 0

let modified_checksum lst = 
  let rec find_tail_blk lst tail_blk = 
    if tail_blk < 0 || tail_blk > Pvector.len lst then failwith "no tail!" else
    match lst --> tail_blk with 
    | Taken _ -> tail_blk
    | _ -> find_tail_blk lst (tail_blk - 1)
  in

  let dot id pos = fun i acc ->  (pos + i) * id + acc in

  let rec h blks head_blk tail_blk pos acc = 
    if head_blk >= Pvector.len blks || head_blk > tail_blk then acc else
    match blks --> head_blk with 
    | Free(free_blk) -> 
      (
        match blks --> tail_blk with 
        | Taken (used_blk) -> 
          if free_blk.len < used_blk.len then 
            (* not enough space in the free block, so consume what we can and update the remaining length of the tail, advance head *)
            let new_blks = (blks, tail_blk) <-- Taken({ used_blk with len = used_blk.len - free_blk.len }) in
            h new_blks (head_blk + 1) tail_blk (pos + free_blk.len) 
              @@ fold_n (dot used_blk.id pos) free_blk.len acc
          else
            (* enough space in the free block, so update its space usage + potentially advance if fully consumed *)
            let new_blks = (blks, head_blk) <-- Free({ len = free_blk.len - used_blk.len }) in 
            let new_head = if free_blk.len = used_blk.len then head_blk + 1 else head_blk in
            h new_blks new_head (find_tail_blk new_blks @@ (tail_blk - 1)) (pos + used_blk.len) 
              @@ fold_n (dot used_blk.id pos) used_blk.len acc
        | _ -> failwith "impossibru!"
      )
    | Taken(blk) -> 
      h blks (head_blk + 1) tail_blk (pos + blk.len) 
        @@ fold_n (dot blk.id pos) blk.len acc
  in
  h lst 0 (find_tail_blk lst @@ (Pvector.len lst - 1)) 0 0

let () = 
"2333133121414131402"
|> parse
|> modified_checksum
|> string_of_int
|> print_endline