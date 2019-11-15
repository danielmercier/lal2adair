exception Lal_error of string

exception Legality_error of string

val lal_error : ('a, Format.formatter, unit, _) format4 -> 'a
(** raise lal error with the given format *)

val legality_error : ('a, Format.formatter, unit, _) format4 -> 'a
(** raise legality error with the given format *)

val compute_name : Libadalang.DefiningName.t -> Ada_ir.Name.t
(** return the Name.t of the given libadalang defining name *)

val eval_as_int : [< Libadalang.Expr.t] -> Ada_ir.Int_lit.t option
(** True evaluating the given expression as an integer *)
