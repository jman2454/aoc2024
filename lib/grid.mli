type 'a t = 'a Pvector.t Pvector.t
val of_string : string -> char t
val ( --> ) : 'a Pvector.t -> int -> 'a
val ( <-- ) : 'a Pvector.t * int -> 'a -> 'a Pvector.t
val at : int * int -> 'a t -> 'a
val in_bounds : int * int -> 'a t -> bool
val at_opt : int * int -> 'a t -> 'a option
val set : int * int -> 'a -> 'a t -> 'a t