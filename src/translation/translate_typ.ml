open Libadalang

let translate_type_of_expr (expr : [< Expr.t]) : IR.Typ.t =
  match Expr.p_expression_type expr with
  | Some base_type_decl ->
      (base_type_decl :> IR.Typ.t)
  | None ->
      Utils.lal_error "cannot find type for expression %s"
        (AdaNode.short_image expr)
