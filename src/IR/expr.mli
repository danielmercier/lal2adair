type 'a with_data = {node: 'a; orig_node: Libadalang.Expr.t; typ: Typ.t}

type t = expr_node with_data

and expr_node =
  | Name of name
  | AttributeRef of attribute_ref
  | Const of const
  | Membership of t * membership_kind * membership_choice list
  | Raise of Name.t * t option
  | Unop of unop * t
  | Binop of binop * t * t
  | RecordAggregate of record_aggregate
  | NullRecordAggregate
  | NamedArrayAggregate of named array_aggregate
  | PositionalArrayAggregate of t array_aggregate
  | Allocator of type_expr * t option
  | If of t * t * t
  | Case of t * case_expr_alternative list * t option
  | Quantified of quantifier * iterator_specification * t

and quantifier = ForAll | Exists

and iterator_specification =
  {var_name: Name.t; reversed: bool; iter_kind: iter_kind}

and iter_kind = Iterator of discrete_range | Iterable of name

and name = name_node with_data

and name_node =
  | Lval of lval
  | Cast of cast with_offset
  | QualExpr of qual_expr with_offset
  | FunctionCall of function_call with_offset
  | AccessOf of access_kind * fun_or_lval

and cast = Typ.t * t

and qual_expr = Typ.t * t

and function_call = function_name * param list

and param =
  | InParam of t
  | OutParam of lval_or_cast with_data
  | InOutParam of lval_or_cast with_data

and lval_or_cast = [`Lval of lval | `Cast of Typ.t * lval_or_cast]

and access_kind = Access | Unchecked_Access | Unrestricted_Access | Address

and lval = base * offset

and base = Var of varinfo | Mem of name

and offset = offset_node with_data

and offset_node =
  | NoOffset
  | Index of offset * t list
  | Slice of offset * discrete_range
  | Field of offset * fieldinfo

and 'a with_offset = 'a * offset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and discrete_range =
  [`DiscreteType of Typ.t * range_constraint option | `Range of range]

and range = DoubleDot of t * t | Range of type_or_name * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and membership_kind = In | NotIn

and membership_choice = [`Expr of t | `Range of range | `Type of Typ.t]

and varinfo = Source of {vname: Name.t; vdecl: vardecl} | Undefined

and vardecl = {decl_scope: decl_scope; decl_kind: decl_kind}

and decl_scope = FunScope of funinfo | PackageScope

and decl_kind =
  | FormalVar of funinfo * varmode
  | ReturnVar
  | ForLoopVar of iterator_specification
  | Variable

and varmode = ModeIn | ModeOut | ModeInOut

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | First of type_or_name * int option
  | Last of type_or_name * int option
  | Length of type_or_name * int option
  | Result of funinfo

and type_or_expr = [`Type of Typ.t | `Expr of t]

and type_or_name = [`Type of Typ.t | `Name of name]

and fun_or_lval = [`Fun of funinfo | `Lval of lval]

and unop = Abs | Not | UnaryMinus | UnaryPlus

and binop =
  | And
  | Or
  | OrElse
  | AndThen
  | Xor
  | Pow
  | Mult
  | Div
  | Mod
  | Rem
  | Plus
  | Minus
  | Concat
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte

and record_aggregate = record_association list

and record_association = {field: fieldinfo; expr: aggregate_expr}

and aggregate_expr = Expr of t | Default

and 'a array_aggregate = {assoc: 'a list; others: t option}

and named = {index: discrete_choice list; aggregate_expr: t}

and discrete_choice =
  [ `Expr of t
  | `DiscreteType of Typ.t * range_constraint option
  | `Range of range ]

and case_expr_alternative = {choices: discrete_choice list; when_expr: t}

val pp : Format.formatter -> t -> unit
