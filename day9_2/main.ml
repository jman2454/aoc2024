type block = { start : int; len : int; id : int }

let char_to_int c = String.make 1 c |> int_of_string

let parse input = 
  let rec h i (used, free, pos) = 
    if i >= String.length input then 
      used, free
    else
      let len = char_to_int (String.get input i) in 
      h (i + 1)
      (
        (* queue-like free list, stack-like used list *)
        if i mod 2 = 0 then 
          { start = pos; len = len; id = i / 2 }::used, free, pos + len
        else
          used, free@[{ start = pos; len = len; id = 0 }], pos + len
      )
  in
  h 0 ([], [], 0)

(* returns the updated block (with its moved position) and the updated free list *)
let rec find_space lst blk = 
  match lst with 
  | slot::rest when slot.start < blk.start && slot.len = blk.len -> { blk with start = slot.start }, rest
  | slot::rest when slot.start < blk.start && slot.len > blk.len -> { blk with start = slot.start }, { slot with len = slot.len - blk.len; start = slot.start + blk.len }::rest
  | hd::rest -> 
    let blk, rest = find_space rest blk in 
    blk, hd::rest
  | [] -> blk, lst

let fold_n fn ct acc = 
  let rec h acc i = 
    if i = ct then acc else 
    h (fn i acc) (i + 1)
  in
  h acc 0

let dot id pos = fun i acc -> (pos + i) * id + acc

let answer input = 
  let used, free = parse input in 
  List.fold_left (fun (used, free) next_used_blk -> 
    let used_blk, free = find_space free next_used_blk in used_blk::used, free
  ) ([], free) used
  |> fst
  |> List.fold_left (fun acc blk -> fold_n (dot blk.id blk.start) blk.len acc) 0

let () = "2333133121414131402"
|> answer
|> string_of_int
|> print_endline