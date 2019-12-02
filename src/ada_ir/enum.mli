type t = {name: name; pos: Int_lit.t}
 and name = string

val pp : Format.formatter -> t -> unit
