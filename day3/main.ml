include Str

let pat = Str.regexp {|mul(\([0-9][0-9]?[0-9]?\),\([0-9][0-9]?[0-9]?\))|}

let parse_mul_expr s i = 
  if Str.string_match pat s i then 
    (print_endline @@ "matched=" ^ (Str.matched_string s);
    let a = Str.matched_group 1 s |> int_of_string in
    let b = Str.matched_group 2 s |> int_of_string in
    a*b, Str.match_end ())
  else 0, i + 1

let answer s = 
  let rec h pos acc = 
    if pos >= String.length s then acc 
    else 
      let v, pos = parse_mul_expr s pos in
      h pos (acc + v)
  in
  h 0 0
  
let () = 

{|xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|}
|> answer
|> string_of_int 
|> print_endline