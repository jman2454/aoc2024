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
module UnionFind :
  functor (Data : RandomAccess) ->
    sig
      type 'a node
      type 'a t
      val of_data : 'a Data.t -> 'a t
      val find : Data.key -> 'a t -> Data.key * 'a t
      val find_no_compress : Data.key -> 'a t -> Data.key
      val union : Data.key -> Data.key -> 'a t -> 'a t
      val same : Data.key -> Data.key -> 'a t -> bool * 'a t
      val same_no_compress : Data.key -> Data.key -> 'a t -> bool
      val at : Data.key -> 'a t -> 'a
      val fold : (Data.key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
      val compress : 'a t -> 'a t
    end

module VectorUnionFind :
  sig
    type 'a node
    type 'a t
    val of_data : 'a Pvector.t -> 'a t
    val find : int -> 'a t -> int * 'a t
    val find_no_compress : int -> 'a t -> int
    val union : int -> int -> 'a t -> 'a t
    val same : int -> int -> 'a t -> bool * 'a t
    val same_no_compress : int -> int -> 'a t -> bool
    val at : int -> 'a t -> 'a
    val fold : (int -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val compress : 'a t -> 'a t
  end
module GridUnionFind :
  sig
    type 'a node
    type 'a t
    val of_data : 'a Grid.t -> 'a t
    val find : int * int -> 'a t -> (int * int) * 'a t
    val find_no_compress : int * int -> 'a t -> int * int
    val union : int * int -> int * int -> 'a t -> 'a t
    val same : int * int -> int * int -> 'a t -> bool * 'a t
    val same_no_compress : int * int -> int * int -> 'a t -> bool
    val at : int * int -> 'a t -> 'a
    val fold : (int * int -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val compress : 'a t -> 'a t
  end

val uf_of_list : 'a list -> 'a VectorUnionFind.t
