type 'a t
val of_pvector : 'a Pvector.t -> 'a t
val of_list : 'a list -> 'a t
val find : int -> 'a t -> int * 'a t
val find_no_compress : int -> 'a t -> int
val union : int -> int -> 'a t -> 'a t
val same : int -> int -> 'a t -> bool * 'a t
val same_no_compress : int -> int -> 'a t -> bool
val at : int -> 'a t -> 'a
val fold : (int -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
val compress : 'a t -> 'a t