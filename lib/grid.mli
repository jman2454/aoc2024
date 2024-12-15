type t
val of_string : string -> t
val at : int * int -> t -> char
val in_bounds : int * int -> t -> bool
val at_opt : int * int -> t -> char option
