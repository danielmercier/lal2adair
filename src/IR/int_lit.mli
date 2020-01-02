type t

val of_int : int -> t
val to_int : t -> int

val pp : Format.formatter -> t -> unit
