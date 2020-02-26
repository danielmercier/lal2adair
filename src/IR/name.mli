open Libadalang

type t = DefiningName.t

module Set : Caml.Set.S with type elt := t

val pp : Format.formatter -> t -> unit
