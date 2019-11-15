open Libadalang

let rec translate_expr (expr : Expr.t) : Ada_ir.Expr.t =
  { node=
      ( match%nolazy expr with
      | #ContractCases.t as contract_cases ->
          translate_contract_cases contract_cases
      | `ParenExpr {f_expr} ->
          (translate_expr (f_expr :> Expr.t)).node
      | #UnOp.t as unop ->
          translate_unop unop
      | #BinOp.t as binop ->
          translate_binop binop
      | #MembershipExpr.t as membership_expr ->
          translate_membership_expr membership_expr
      | #BaseAggregate.t as base_aggregate ->
          translate_base_aggregate base_aggregate
      | #Name.t as name ->
          translate_name name
      | #BoxExpr.t as box_expr ->
          translate_box_expr box_expr
      | #IfExpr.t as if_expr ->
          translate_if_expr if_expr
      | #CaseExpr.t as case_expr ->
          translate_case_expr case_expr
      | #CaseExprAlternative.t as case_expr_alternative ->
          translate_case_expr_alternative case_expr_alternative
      | #QuantifiedExpr.t as quantified_expr ->
          translate_quantified_expr quantified_expr
      | #Allocator.t as allocator ->
          translate_allocator allocator
      | #RaiseExpr.t as raise_expr ->
          translate_raise_expr raise_expr )
  ; orig_node= expr
  ; typ= Translate_typ.translate_type_of_expr expr }

and translate_contract_cases (_contract_cases : ContractCases.t) = assert false

and translate_unop (_unop : UnOp.t) = assert false

and translate_binop (_binop : BinOp.t) = assert false

and translate_membership_expr (_membership_expr : MembershipExpr.t) =
  assert false

and translate_base_aggregate (_base_aggregate : BaseAggregate.t) = assert false

and translate_name (name : Name.t) : Ada_ir.Expr.expr_node =
  let open Ada_ir.Expr in
  match%nolazy name with
  | #IntLiteral.t as int_literal ->
      Const
        (Int (Ada_ir.Int_lit.of_int (IntLiteral.p_denoted_value int_literal)))
  | #StringLiteral.t as string_literal ->
      Const (String (StringLiteral.p_denoted_value string_literal))
  | #NullLiteral.t ->
      Const Null
  | #CharLiteral.t as char_lit ->
      (* A char literal is a regular enum. Use p_eval_as_int which correctly
         evaluates the position of the enum *)
      let name = AdaNode.text char_lit in
      let pos = Expr.p_eval_as_int char_lit in
      Const (Enum {Ada_ir.Enum.name; pos})

and translate_box_expr (_box_expr : BoxExpr.t) = assert false

and translate_if_expr (_if_expr : IfExpr.t) = assert false

and translate_case_expr (_case_expr : CaseExpr.t) = assert false

and translate_case_expr_alternative
    (_case_expr_alternative : CaseExprAlternative.t) =
  assert false

and translate_quantified_expr (_quantified_expr : QuantifiedExpr.t) =
  assert false

and translate_allocator (_allocator : Allocator.t) = assert false

and translate_raise_expr (_raise_expr : RaiseExpr.t) = assert false
