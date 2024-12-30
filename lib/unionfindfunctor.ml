module type RandomAccess = 
sig 
  type 'a t
  type key

  val at : key -> 'a t -> 'a
  val set : key -> 'a -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val keyed_map : (key -> 'a -> 'b) -> 'a t -> 'b t
  val keyed_fold : ('acc -> key -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module UnionFind(Data : RandomAccess) = 
struct
  (* add-only, persistent + immutable datastructure *)
  type 'a node = { value : 'a; parent : Data.key } (* depth : int option *)
    (* nvm, depth should be in the 'a t type (make it have the Data.t and depth : int) *)

  type 'a t = 'a node Data.t

  let of_data data : 'a t = Data.keyed_map (fun key value -> { value = value; parent = key }) data

  let (-->) d k = Data.at k d
  let (<--) (d, k) el = Data.set k el d

  (* returns a token identifying the set containing the element at index i, and the (maybe) updated union find *)
  let find i (uf : 'a t) = 
    let rec h i = (* add extra acc in h that indicates max depth *)
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

  (* store the depth in the root? *)

  (* merges the sets at the specified indices *)
  let union i_l i_r (uf : 'a t) : 'a t = 
    let l_rep, uf = find i_l uf in 
    let r_rep, uf = find i_r uf in 
    if l_rep <> r_rep then 
      (* union by rank here -- set the rep element of the deeper tree to be that of the shallower tree *)
      (uf, r_rep) <-- { (uf --> r_rep) with parent = l_rep }
    else
      uf

  (* returns true if i_l and i_r are in the same set, false otherwise and the (maybe) updated data structure *)
  let same i_l i_r uf : bool * 'a t = 
    if not @@ Data.mem i_l uf || not @@ Data.mem i_r uf then false, uf else
    let l_rep, uf = find i_l uf in 
    let r_rep, uf = find i_r uf in 
    l_rep = r_rep, uf

  (* returns true if i_l and i_r are in the same set, false otherwise *)
  let same_no_compress i_l i_r uf : bool = 
    if not @@ Data.mem i_l uf || not @@ Data.mem i_r uf then false else
    find_no_compress i_l uf = find_no_compress i_r uf

  let at i (uf : 'a t) = (uf --> i).value

  let fold fn acc uf = Data.keyed_fold (fun acc i node -> fn i node.value acc) acc uf

  (* fully compresses the data structure -- best to do once done adding elements *)
  let compress (uf : 'a t) = Data.keyed_fold (fun uf i _ -> find i uf |> snd) uf uf
end

module VectorUnionFind = UnionFind(struct 
  type 'a t = 'a Pvector.t
  type key = int 
  let at = Pvector.at
  let set = Pvector.set
  let mem i vec = i > -1 && i < Pvector.len vec
  let keyed_map = Pvector.mapi
  let keyed_fold = Pvector.fold_lefti
end)

module GridUnionFind = UnionFind(struct 
  type 'a t = 'a Grid.t
  type key = int * int
  let at = Grid.at
  let set = Grid.set
  let mem = Grid.in_bounds
  let keyed_map = Grid.mapi
  let keyed_fold = Grid.fold_lefti
end)

(* Indices from the original list provide access to elements in the union find -- so users must track them *)

let uf_of_list (lst : 'a list) : 'a VectorUnionFind.t = VectorUnionFind.of_data (Pvector.of_list lst)