type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node = Const of const

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and name = {plain: string; mangled: string}
