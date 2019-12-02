open Libadalang

type t = [BaseTypeDecl.t | TypeExpr.t]

and desc =
  | Discrete of discrete_typ
  | Real of real_typ
  | Access of access_typ
  | Array of array_typ
  | Record of record_typ

and discrete_typ =
  | Enum of enum_typ
  | Signed of bound * bound
  | Modular of Int_lit.t

and enum_typ =
  | Character of Int_lit.t (* Length of the character type *)
  | Boolean
  | OtherEnum of Enum.name list

(** A bound can either be statically known or dynamic, in which case a variable
    should be used to denote the bound in the frontend *)
and bound = Static of static_expr | Dynamic

and static_expr = Int_lit.t

and access_typ = Object of t | Subprogram of subprogram_typ

and subprogram_typ =
  | Function of {name: subprogram_name; args: var_info list; ret_typ: t}
  | Procedure of {name: subprogram_name; args: var_info list}

and var_info = {var_name: Name.t; var_typ: t}

and subprogram_name = Name.t

and array_typ =
  {elt_typ: t; indicies: discrete_typ list; constrained: constrained}

and constrained = Constrained | Unconstrained

and real_typ = Float | Fixed

and record_typ = {discriminants: discriminant_list; fields: field list}

and discriminant_list =
  | ConstrainedDiscr of
      (Name.t * [`Discrete of discrete_typ | `Access of access_typ] * bound)
      list
  | UnconstrainedDiscr of
      (Name.t * [`Discrete of discrete_typ | `Access of access_typ]) list

and field =
  {field_name: Name.t; field_typ: t; field_constraint: field_constraint list}

and field_constraint = {discriminant: Name.t; constr: constr}

and constr =
  | Range of static_expr * static_expr
  | Equal of static_expr
  | Alternatives of constr list
  | Not of constr

let desc = assert false
