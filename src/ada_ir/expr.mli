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
  | DiscreteType of Typ.t * range_constraint option
  | DiscreteRange of range

and range = DoubleDot of t * t | Range of range_prefix * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and range_prefix = Type of Typ.t | Array of name

and membership_kind = In | NotIn

and membership_choice =
  | ChoiceExpr of t
  | ChoiceRange of range
  | ChoiceType of Typ.t

and varinfo = Source of {vname: Name.t} | Undefined

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | NameAccess of access_kind * name
  | FunAccess of access_kind * funinfo

and access_kind = Access | Unchecked_Access | Unrestriced_Access | Address

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

and discrete_choice = ExprChoice of t | RangeChoice of discrete_range

val undefined : unit -> name
(** return a undefined name *)

val pp : Format.formatter -> t -> unit
