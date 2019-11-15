open Libadalang

let translate_type_of_expr (expr : [< Expr.t]) : Ada_ir.Typ.t =
  match Expr.p_expression_type expr with
  | Some base_type_decl ->
      {decl_node= (base_type_decl :> Ada_ir.Typ.lal_type)}
  | None ->
      Utils.lal_error "cannot find type for expression %s"
        (AdaNode.short_image expr)

(*let rec translate_base_type_decl (base_type_decl : BaseTypeDecl.t) :
    Ada_ir.Typ.t =
  match base_type_decl with
  | #TypeDecl.t | #DiscreteBaseSubtypeDecl.t ->
      {decl_node= base_type_decl; type_constraint= None}
  | #SubtypeDecl.t as subtype_decl ->
      translate_subtype_decl subtype_decl

and translate_subtype_decl (subtype_decl : SubtypeDecl.t) : Ada_ir.Typ.t =
  translate_subtype_indication (SubtypeDecl.f_subtype subtype_decl)

and translate_subtype_indication (subtype : SubtypeIndication.t) : Ada_ir.Typ.t
    =
  let name = SubtypeIndication.f_name subtype in
  let constr = SubtypeIndication.f_constraint subtype in
  match Name.p_referenced_decl name with
  | Some (#BaseTypeDecl.t as base_type_decl) ->
      {decl_node= base_type_decl; type_constraint= translate_type_constraint constr}
  | Some decl ->
      Utils.legality_error "expecting a type for subtype indication %s, got %s" (AdaNode.short_image subtype) (AdaNode.short_image decl)
  | None ->
      Utils.lal_error "name resolution failed for %s" (AdaNode.short_image name)

and translate_type_constraint (type_constraint : Constraint.t option) : Ada_ir.Typ.type_constraint =
  match type_constraint with
  | Some (`RangeConstraint f_range)
  | None -> None

and translate_range_spec (range_spec : RangeSpec.t) : Ada_ir.Typ.bound * Ada_ir.Typ.bound =
  translate_range (RangeSpec.f_range range_spec :> Expr.t)

and translate_range (range : Expr.t) : Ada_ir.Typ.bound * Ada_ir.Typ.bound =
  match%nolazy range with
  | `RelationOp {f_left; f_op= `OpDoubleDot _; f_right} ->
      translate_bound (f_left :> Expr.t), translate_bound (f_right :> Expr.t)

and translate_bound (expr : Expr.t) : Ada_ir.Typ.bound = *)
