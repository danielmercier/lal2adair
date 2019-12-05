type t = {name: name; pos: Int_lit.t}

(* We need a Std char literal because name resolution for char literals in
   lal fails for char literals of the standard library *)
and name = EnumLiteral of Name.t | StdCharLiteral of string

val pp : Format.formatter -> t -> unit
