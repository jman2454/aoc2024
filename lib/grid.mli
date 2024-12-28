type 'a t = 'a Pvector.t Pvector.t
val of_string : string -> char t
val at : int * int -> 'a t -> 'a
val in_bounds : int * int -> 'a t -> bool
val at_opt : int * int -> 'a t -> 'a option
val set : int * int -> 'a -> 'a t -> 'a t
val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val flipped_mapi : (int * int -> 'a -> 'b) -> 'a t -> 'b t
val mapi : (int * int -> 'a -> 'b) -> 'a t -> 'b t