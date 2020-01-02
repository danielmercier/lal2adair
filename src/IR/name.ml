open Libadalang

type t = DefiningName.t

let pp fmt name = Format.pp_print_string fmt (AdaNode.text name)
