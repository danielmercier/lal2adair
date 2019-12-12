open Libadalang

(** StringLiteral is part of an identifier because it can refer to an
    symbol operator *)
type identifier = [DottedName.t | Identifier.t | StringLiteral.t]

type literal =
  [ IntLiteral.t
  | StringLiteral.t
  | NullLiteral.t
  | CharLiteral.t
  | RealLiteral.t
  | identifier ]

type call = [identifier | CallExpr.t | ExplicitDeref.t]

type range = [BinOp.t | AttributeRef.t]

type discrete_range = [identifier | range | DiscreteSubtypeIndication.t]

val is_range : [< range] -> bool
(** return true if the given range is a legal range. A legal range is either
    a binop with double dot operator or a range attribute reference *)

val is_discrete_range : [< discrete_range] -> bool
(** return true if the given discrete range is a legal range. A legal discrete
    range is either range or subtype indication *)

val is_literal : [< literal] -> bool
(** return true if the given literal truelly denotes a literal. i.e. if the
    given literal is an identifier, return true if it denotes an enum *)

val is_variable : [< identifier] -> bool
(** return true if the given identifier is a variable *)

val is_record_access : DottedName.t -> bool
(** return true if the given dotted name is an access to a record field *)

val is_lvalue : [< Name.t] -> bool
(** return true if the given name is an lvalue *)

val is_subprogram : [< identifier] -> bool
(** return true if the given identifier is a constant subprogram *)

val is_call : [< call] -> bool
(** return true if the given node is a call. Be carefull, this does not take
    into account the context. It just looks at the declaration of the given
    node. *)

val is_access_type : [< BaseTypeDecl.t] -> bool
(** return true if the given type is access looking at the full view *)
