open Libadalang

type t = DefiningName.t

module Set = Caml.Set.Make (DefiningName)

let pp fmt name = Format.pp_print_string fmt (AdaNode.text name)
