type t = data expr

and data = {orig_node: Libadalang.AdaNode.t; typ: Typ.t}

and 'a expr = {node: expr_node; data: 'a}

and expr_node = Const of const

and const = Int of int | String of string | Null | Enum of Enum.t
