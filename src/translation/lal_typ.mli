open Libadalang

type literal =
  [ IntLiteral.t
  | StringLiteral.t
  | NullLiteral.t
  | CharLiteral.t
  | RealLiteral.t ]

type identifier = [DottedName.t | Identifier.t]

type call = [CallExpr.t | DottedName.t | Identifier.t | ExplicitDeref.t]

val is_variable : identifier -> bool
(** return true if the given identifier is a variable *)

val is_record_access : DottedName.t -> bool
(** return true if the given dotted name is an access to a record field *)

val is_lvalue : Name.t -> bool
(** return true if the given name is an lvalue *)

val is_subprogram : identifier -> bool
(** return true if the given identifier is a constant subprogram *)

val is_call : call -> bool
(** return true if the given node is a call. Be carefull, this does not take
    into account the context. It just looks at the declaration of the given
    node. *)
