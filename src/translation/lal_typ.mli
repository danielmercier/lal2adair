open Libadalang

type identifier = [DottedName.t | Identifier.t]

type literal =
  [ IntLiteral.t
  | StringLiteral.t
  | NullLiteral.t
  | CharLiteral.t
  | RealLiteral.t
  | identifier ]

type call = [CallExpr.t | DottedName.t | Identifier.t | ExplicitDeref.t]

val is_literal : literal -> bool
(** return true if the given literal truelly denotes a literal. i.e. if the
    given literal is an identifier, return true if it denotes an enum *)

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

val is_access_type : [< BaseTypeDecl.t] -> bool
(** return true if the given type is access looking at the full view *)
