include Pvector

(* add-only datastructure *)
type 'a node = { value : 'a; parent : int }
type 'a t = 'a node Pvector.t

(* Indices from the original list provide access to elements in the union find -- so users must track them *)
let of_pvector (vec : 'a Pvector.t) : 'a t = Pvector.mapi (fun i el -> { value = el; parent = i }) vec
let of_list (lst : 'a list) : 'a t = of_pvector (Pvector.of_list lst)

(* returns a token identifying the set containing the element at index i, and the (maybe) updated union find *)
let find i (uf : 'a t) = 
  let rec h i = 
    let el = uf --> i in 
    if el.parent = i then i, uf else 
      let p_i, uf = h el.parent in 
      p_i, (uf, i) <-- { el with parent = p_i }
  in
  h i

(* same as find, but does not ever update the data structure -- best to use after updating with compress *)
let find_no_compress i (uf : 'a t) = 
  let rec h i = 
    let el = uf --> i in 
    if el.parent = i then i else h el.parent
  in
  h i

(* merges the sets at the specified indices *)
let union i_l i_r (uf : 'a t) : 'a t = 
  let l_rep, uf = find i_l uf in 
  let r_rep, uf = find i_r uf in 
  if l_rep <> r_rep then 
    (uf, r_rep) <-- { (uf --> r_rep) with parent = l_rep }
  else
    uf

(* returns true if i_l and i_r are in the same set, false otherwise and the (maybe) updated data structure *)
let same i_l i_r uf : bool * 'a t = 
  let l_rep, uf = find i_l uf in 
  let r_rep, uf = find i_r uf in 
  l_rep = r_rep, uf

(* returns true if i_l and i_r are in the same set, false otherwise *)
let same_no_compress i_l i_r uf : bool = find_no_compress i_l uf = find_no_compress i_r uf

let at i (uf : 'a t) = (uf --> i).value

let fold fn acc uf = Pvector.fold_lefti (fun acc i node -> fn (find_no_compress i uf) node.value acc) acc uf

(* fully compresses the data structure -- best to do once done adding elements *)
let compress (uf : 'a t) = Pvector.fold_lefti (fun uf i _ -> find i uf |> snd) uf uf