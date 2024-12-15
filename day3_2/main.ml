include Str

let mul = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}
let disable = Str.regexp {|don't()|}
let enable = Str.regexp {|do()|}

let find_match pat s i = 
  let rec h i = 
    if i >= String.length s then None
    else if Str.string_match pat s i then Some(Str.match_end ())
    else h (i + 1)
  in
  h i

let rec eval s i acc can_mul = 
  if can_mul then 
    match find_match disable s i, find_match mul s i with 
    | Some(disable_pos), Some(mul_pos) when disable_pos < mul_pos -> eval s disable_pos acc false 
    | _, Some(mul_pos) -> 
      let a = Str.matched_group 1 s |> int_of_string in
      let b = Str.matched_group 2 s |> int_of_string in 
      eval s mul_pos (acc + a*b) true
    | _, _ -> acc
  else
    match find_match enable s i with 
    | Some(pos) -> eval s pos acc true
    | None -> acc

let answer s = eval s 0 0 true
  
let () = 
{|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}
|> answer
|> string_of_int 
|> print_endline