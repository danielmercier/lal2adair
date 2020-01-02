open Libadalang

exception Lal_error of string

exception Legality_error of string

val lal_error : ('a, Format.formatter, unit, _) format4 -> 'a
(** raise lal error with the given format *)

val legality_error : ('a, Format.formatter, unit, _) format4 -> 'a
(** raise legality error with the given format *)

val log_warning : ('a, Format.formatter, unit, unit) format4 -> 'a
(** log the given warning *)

val try_or_undefined :
  string -> (([< AdaNode.t] as 'a) -> IR.Expr.expr_node) -> 'a -> IR.Expr.expr_node
(** if the given function raise an exception, log a message and return a
    undefined expression *)

val unimplemented : [< AdaNode.t] -> IR.Expr.name
(** log a warning about not implemented node and return undefined *)

val defining_name : [< Lal_typ.identifier] -> IR.Name.t
(** given a lal name, return a defining name using name resolution *)

val referenced_subp_spec : [< Name.t] -> BaseSubpSpec.t
(** assuming given name refers to a subprogram, return its specification.
    Otherwise, raise a legality error *)

val accessed_subp_spec : [< Name.t] -> BaseSubpSpec.t
(** assuming given name refers to an access to subprogram, return its
    specification.  Otherwise, raise a legality error *)

type attribute =
  [ `Access
  | `Unchecked_Access
  | `Unrestricted_Access
  | `Address
  | `Length
  | `First
  | `Last
  | `Range
  | `Val
  | `Pos
  | `Succ
  | `Pred
  | `Result
  | `Unknown of string ]

val attribute : Identifier.t -> attribute
(** Return attribute that the identifier is referring to *)

val pp_node : Format.formatter -> [< AdaNode.t] -> unit
(** Type for various ada attributes *)
