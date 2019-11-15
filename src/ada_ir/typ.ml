type t = {node: typ_node; orig_node: Libadalang.AdaNode.t}

and typ_node =
  | Scalar of scalar_typ
  | Access of access_typ
  | Composite of composite_typ

and scalar_typ = Discrete of discrete_typ | Real of real_typ

and discrete_typ =
  | Enum of Enum.name list
  | Signed of bound * bound
  | Modular of int

(** A bound can either be statically known or dynamic, in which case a variable
    should be used to denote the bound in the frontend *)
and bound = Static of Int_lit.t | Dynamic

and access_typ = Object of t | Subprogram of subprogram_typ

and subprogram_typ =
  | Function of {name: subprogram_name; args: var_info list; ret_typ: t}
  | Procedure of {name: subprogram_name; args: var_info list}

and var_info = {var_name: string; var_typ: t}

and subprogram_name = string

and composite_typ = Array of array_typ | Record of record_typ

and array_typ =
  {elt_typ: t; indicies: discrete_typ list; constrained: constrained}

and constrained = Constrained | Unconstrained

and real_typ = Float | Fixed

and record_typ = {fields: field list}

and field = {name: string; typ: t}
