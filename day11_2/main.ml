(* basically conway's game of life, but easier *)
(* I don't think we care about positionality of the stones *)
(*** 
  step rules (from the challenge):
  1) If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
  2) If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
    The left half of the digits are engraved on the new left stone, and the right half of the digits are 
    engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
  3) If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is 
    engraved on the new stone
***)

let num_digits n = 
  log10 (float_of_int n)
  |> floor
  |> int_of_float
  |> (+) 1

let even_digits n = (num_digits n) mod 2 = 0

let pow n m = 
  let rec h i acc = 
    if i = 0 then acc else h (i - 1) (acc * n)
  in
  h m 1

let split_in_half n = 
  let m = pow 10 (num_digits n / 2) in 
  (n mod m, n / m)

module IntMap = Map.Make(Int)

(* maybe we just use a map / counter of number incidence? *)

let step stones = 
  let combine_cts ct = (Option.fold ~none:(Some(ct)) ~some:(fun c -> Some(c+ct))) in 
  IntMap.fold (fun stone ct stones -> 
    let count_update = combine_cts ct in
    if stone = 0 then 
      IntMap.update 1 count_update stones
    else if even_digits stone then 
      let a, b = split_in_half stone in 
      IntMap.update a count_update stones
      |> IntMap.update b count_update
    else
      IntMap.update (2024*stone) count_update stones
  ) stones IntMap.empty

let do_n fn ct acc = 
  let rec h i acc = 
    if i = ct then acc else 
    h (i + 1) (fn acc)
  in
  h 0 acc

let answer input = 
  (String.split_on_char ' ' input
  |> List.map int_of_string
  |> List.fold_left (fun map value -> IntMap.update value (fun x -> Some(1 + Option.value ~default:0 x)) map) IntMap.empty
  |> do_n step 75
  |> IntMap.fold (fun _ ct acc -> ct + acc)) 0

let () = 
"773 79858 0 71 213357 2937 1 3998391"
|> answer
|> string_of_int
|> print_endline