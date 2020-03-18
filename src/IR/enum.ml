type t = {name: name; pos: Int_lit.t}

and name = EnumLiteral of Name.t | StdCharLiteral of string

let pp_name fmt name =
  match name with
  | EnumLiteral name ->
      Name.pp fmt name
  | StdCharLiteral str ->
      Format.pp_print_string fmt str


let pp fmt {name} = pp_name fmt name
