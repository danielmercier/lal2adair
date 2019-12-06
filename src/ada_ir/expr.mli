type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Const of const
  | Lval of lval
  | CallExpr of called_expr * t list
  | AccessOf of access_kind * lval
  | Cast of Typ.t * t

and lval = lhost * offset

and lhost =
  | Var of varinfo
  | CustomVar of custom_var
  (* It is possible in Ada, to get the field after calling a function *)
  | CallHost of called_expr * t list
  | Mem of t

and offset =
  | Field of fieldinfo * offset
  | Index of t list * offset
  | Slice of range * offset
  | NoOffset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and range =
  | TypeExpr of (Typ.t * range_constraint option)
  | DoubleDot of t * t
  | Range of range_prefix * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_prefix = Type of Typ.t | Array of lval

and range_constraint = t * t

and varinfo = {vname: Name.t}

and custom_var = Undefined

and fieldinfo = {fieldname: Name.t}

and called_expr = Cfun of funinfo | Pfun of t

and funinfo = {fname: Name.t}

and access_kind = Access | Unchecked_Access | Unrestriced_Access | Address

val undefined : unit -> expr_node
(** return a undefined expression *)

val pp : Format.formatter -> t -> unit
