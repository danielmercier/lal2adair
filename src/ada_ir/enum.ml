type t = {name: name; pos: Int_lit.t}

and name = string

let pp fmt {name} = Format.pp_print_string fmt name
