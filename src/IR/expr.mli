type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Name of name
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

and name =
  | Var of varinfo
  | Enum of Enum.t
  | Deref of name
  | Index of name * t list
  | Slice of name * discrete_range
  | Field of name * fieldinfo
  | AttributeRef of attribute_ref
  | Cast of Typ.t * t
  | FunctionCall of function_name * t list
  | QualExpr of Typ.t * t

and const = Int of Int_lit.t | String of string | Null

and discrete_range =
  [`DiscreteType of Typ.t * range_constraint option | `Range of range]

and range = DoubleDot of t * t | Range of type_or_name * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and membership_kind = In | NotIn

and membership_choice = [`Expr of t | `Range of range | `Type of Typ.t]

and varinfo = Source of {vname: Name.t} | Undefined

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | Access of fun_or_name
  | Unchecked_Access of fun_or_name
  | Unrestricted_Access of fun_or_name
  | Address of fun_or_name
  | First of type_or_name * int option
  | Last of type_or_name * int option
  | Length of type_or_name * int option
  | Result of funinfo

and type_or_expr = [`Type of Typ.t | `Expr of t]

and type_or_name = [`Type of Typ.t | `Name of name]

and fun_or_name = [`Fun of funinfo | `Name of name]

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

val undefined : unit -> name
(** return a undefined name *)

val pp : Format.formatter -> t -> unit
