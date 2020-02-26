open Libadalang

(** translate an expression to a name checking for an implicit deref *)
let name_from_expr (check_implicit : bool) (expr : IR.Expr.t) :
    IR.Expr.name option =
  match expr.node with
  | Name name ->
      if check_implicit then
        match expr.typ.desc with
        | IR.Typ.Access _ ->
            (* Implicit deref *)
            Some {expr with node= Lval (Mem name, {expr with node= NoOffset})}
        | _ ->
            Some name
      else Some name
  | _ ->
      None


let funinfo (spec : [< BaseSubpSpec.t]) =
  match%nolazy (spec :> BaseSubpSpec.t) with
  | `SubpSpec {f_subp_name= Some fname} ->
      IR.Expr.{fname}
  | _ ->
      Utils.lal_error "Cannot find name for subprogram spec %a" Utils.pp_node
        spec


let referenced_funinfo (expr : Expr.t) =
  match expr with
  | #Lal_typ.identifier as ident when Lal_typ.is_subprogram ident ->
      let subp_spec = Utils.referenced_subp_spec ident in
      Some (funinfo subp_spec)
  | _ ->
      None


let fieldinfo (ident : Lal_typ.identifier) : IR.Expr.fieldinfo =
  {fieldname= Utils.defining_name ident}


let append_offset ~f name =
  (* If the given name can be offseted, call f with current offset of name *)
  match name with
  | IR.Expr.Lval (base, offset) ->
      let offset = f offset in
      Some {offset with IR.Expr.node= IR.Expr.Lval (base, offset)}
  | Cast (base, offset) ->
      let offset = f offset in
      Some {offset with node= Cast (base, offset)}
  | QualExpr (base, offset) ->
      let offset = f offset in
      Some {offset with node= QualExpr (base, offset)}
  | FunctionCall (base, offset) ->
      let offset = f offset in
      Some {offset with node= FunctionCall (base, offset)}
  | AccessOf _ ->
      None


let convert_mode mode =
  match mode with
  | Some #ModeIn.t | Some #ModeDefault.t | None ->
      IR.Expr.ModeIn
  | Some #ModeInOut.t ->
      ModeInOut
  | Some #ModeOut.t ->
      ModeOut


type type_context = {tenv: IR.Typ.tenv; translating: IR.Name.Set.t}

let from_tenv tenv = {tenv; translating= IR.Name.Set.empty}

let global_tenv = IR.Typ.mk_empty_tenv ()

let global_ctx = from_tenv global_tenv

let mk orig_node desc =
  { IR.Typ.desc
  ; aspects= []
  ; name= Name (Utils.decl_defining_name orig_node)
  ; parent_type= None
  ; orig_node= (orig_node :> BaseTypeDecl.t) }


let int64_desc =
  let min_int = IR.Int_lit.of_string "-0x8000000000000000" in
  let max_int = IR.Int_lit.of_string "0x7FFFFFFFFFFFFFFF" in
  IR.Typ.Discrete (Signed (min_int, max_int), None)


let get_aspects basic_decl =
  try
    if (BasicDecl.p_get_aspect basic_decl "volatile").exists then
      [IR.Typ.Volatile]
    else []
  with _ -> []


let merge_aspects l_aspects r_aspects =
  List.dedup_and_sort ~compare:Pervasives.compare (l_aspects @ r_aspects)


let char_desc = IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 256)), None)

let wide_char_desc =
  IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 65536)), None)


let wide_wide_char_desc =
  IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 2147483648)), None)


let to_int (expr : IR.Expr.t) =
  (* Return a int literal from an expression, raise a legality error if the
     expression is not static *)
  match expr.node with
  | Const (Int i) ->
      i
  | _ ->
      Utils.legality_error "Expected a static expression, got %a" Utils.pp_node
        expr.orig_node


let with_data (orig_node : [< Expr.t]) (typ : IR.Typ.t) (node : 'a) =
  {IR.Expr.node; orig_node= (orig_node :> Expr.t); typ}


(** Make a NoOffset node from given data *)
let no_offset orig_node typ = with_data orig_node typ IR.Expr.NoOffset

let undefined node typ =
  with_data node typ
    (IR.Expr.Name
       (with_data node typ (IR.Expr.Lval (Var Undefined, no_offset node typ))))


let try_or_undefined property f node typ =
  try with_data node typ (f node)
  with PropertyError s ->
    Utils.log_warning "Cannot evaluate %a, PropertyError on %s: %s"
      Utils.pp_node node property s ;
    undefined node typ


let unimplemented = undefined

let rec translate_expr (expr : Expr.t) : IR.Expr.t =
  match%nolazy expr with
  | #ContractCases.t as contract_cases ->
      translate_contract_cases contract_cases
  | `ParenExpr {f_expr} ->
      translate_expr (f_expr :> Expr.t)
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
      translate_raise_expr raise_expr


and translate_variable (var : Lal_typ.identifier) =
  let vname = Utils.defining_name var in
  let vdecl =
    match Name.p_referenced_decl var with
    | Some decl ->
        translate_vardecl decl
    | None ->
        Utils.lal_error "Cannot find declaration for %a" Utils.pp_node var
  in
  let typ = translate_type_of_expr global_ctx (var :> Expr.t) in
  let offset = no_offset var typ in
  with_data var typ IR.Expr.(Lval (Var (Source {vname; vdecl}), offset))


and translate_vardecl (decl : BasicDecl.t) =
  let decl_scope =
    (* Return the proc in which a declaration is in if it exists *)
    let rec f = function
      | #BaseSubpBody.t as subp ->
          let subp_spec = BaseSubpBody.f_subp_spec subp in
          Some (funinfo subp_spec)
      | node ->
          Option.bind ~f (AdaNode.parent node)
    in
    match Option.bind ~f (AdaNode.parent decl) with
    | Some funinfo ->
        IR.Expr.FunScope funinfo
    | None ->
        PackageScope
  in
  let decl_kind =
    match%nolazy decl with
    | `ParamSpec {f_mode} ->
        let rec find_funinfo = function
          | #SubpSpec.t as subp_spec ->
              funinfo subp_spec
          | node -> (
            match AdaNode.parent node with
            | Some parent ->
                find_funinfo parent
            | None ->
                Utils.lal_error
                  "Cannot find subprogram specification for subprogram \
                   parameter %a"
                  Utils.pp_node decl )
        in
        IR.Expr.FormalVar
          (find_funinfo (decl :> AdaNode.t), convert_mode f_mode)
    | #ExtendedReturnStmtObjectDecl.t ->
        ReturnVar
    | #ForLoopVarDecl.t ->
        let iterator_spec =
          match AdaNode.parent decl with
          | Some (#ForLoopSpec.t as loop_spec) ->
              translate_iterator_specification loop_spec
          | _ ->
              Utils.lal_error
                "Cannot find loop specification for loop var decl %a"
                Utils.pp_node decl
        in
        ForLoopVar iterator_spec
    | _ ->
        Variable
  in
  {IR.Expr.decl_scope; decl_kind}


and translate_record_access (dotted_name : DottedName.t) : IR.Expr.name =
  let typ = translate_type_of_expr global_ctx (dotted_name :> Expr.t) in
  match DottedName.f_suffix dotted_name with
  | #Identifier.t as ident -> (
      let prefix = (DottedName.f_prefix dotted_name :> Expr.t) in
      let prefix_expr = translate_expr prefix in
      let name =
        match name_from_expr true prefix_expr with
        | Some name ->
            name
        | None ->
            Utils.legality_error "Cannot access a field of a non lvalue: %a"
              Utils.pp_node prefix
      in
      let f offset =
        with_data dotted_name typ (IR.Expr.Field (offset, fieldinfo ident))
      in
      match append_offset ~f name.node with
      | Some name ->
          name
      | None ->
          Utils.legality_error "Cannot access the field for %a" Utils.pp_node
            prefix )
  | suffix ->
      (* For a record access, the only possible suffix is an identifier *)
      Utils.lal_error "Expecting an identifier for field, found %a"
        Utils.pp_node suffix


and translate_contract_cases (_contract_cases : ContractCases.t) = assert false

and translate_unop (unop : UnOp.t) =
  let typ = translate_type_of_expr global_ctx (unop :> Expr.t) in
  let op = UnOp.f_op unop in
  let expr = translate_expr (UnOp.f_expr unop :> Expr.t) in
  (* First check if there is a user defined operator *)
  match try Name.p_referenced_decl op with _ -> None with
  | Some _ ->
      let subp_spec = Utils.referenced_subp_spec op in
      let function_call =
        IR.Expr.FunctionCall
          ((Cfun (funinfo subp_spec), [InParam expr]), no_offset unop typ)
      in
      let name_node = IR.Expr.Name (with_data unop typ function_call) in
      with_data unop typ name_node
  | None ->
      let operator =
        match op with
        | #OpAbs.t ->
            IR.Expr.Abs
        | #OpNot.t ->
            Not
        | #OpMinus.t ->
            UnaryMinus
        | #OpPlus.t ->
            UnaryPlus
      in
      let expr_node = IR.Expr.Unop (operator, expr) in
      with_data unop typ expr_node


and translate_binop (binop : BinOp.t) =
  let typ = translate_type_of_expr global_ctx (binop :> Expr.t) in
  let op = BinOp.f_op binop in
  let lexpr = translate_expr (BinOp.f_left binop :> Expr.t) in
  let rexpr = translate_expr (BinOp.f_right binop :> Expr.t) in
  match try Name.p_referenced_decl op with _ -> None with
  | Some _ ->
      let subp_spec = Utils.referenced_subp_spec op in
      let function_call =
        IR.Expr.FunctionCall
          ( (Cfun (funinfo subp_spec), [InParam lexpr; InParam rexpr])
          , no_offset binop typ )
      in
      let name_node = IR.Expr.Name (with_data binop typ function_call) in
      with_data binop typ name_node
  | None ->
      let operator =
        match op with
        | #OpAnd.t ->
            IR.Expr.And
        | #OpOr.t ->
            Or
        | #OpOrElse.t ->
            OrElse
        | #OpAndThen.t ->
            AndThen
        | #OpXor.t ->
            Xor
        | #OpPow.t ->
            Pow
        | #OpMult.t ->
            Mult
        | #OpDiv.t ->
            Div
        | #OpMod.t ->
            Mod
        | #OpRem.t ->
            Rem
        | #OpPlus.t ->
            Plus
        | #OpMinus.t ->
            Minus
        | #OpConcat.t ->
            Concat
        | #OpEq.t ->
            Eq
        | #OpNeq.t ->
            Neq
        | #OpLt.t ->
            Lt
        | #OpLte.t ->
            Lte
        | #OpGt.t ->
            Gt
        | #OpGte.t ->
            Gte
        | _ ->
            Utils.legality_error "Unexpected binary operator %a" Utils.pp_node
              op
      in
      let expr_node = IR.Expr.Binop (operator, lexpr, rexpr) in
      with_data binop typ expr_node


and translate_membership_expr (membership_expr : MembershipExpr.t) =
  let translate_membership_choice expr =
    match expr with
    | #Lal_typ.range as range when Lal_typ.is_range range ->
        `Range (translate_range range)
    | _ ->
        (translate_type_or_expr (expr :> Expr.t) :> IR.Expr.membership_choice)
  in
  let prefix_expr =
    translate_expr (MembershipExpr.f_expr membership_expr :> Expr.t)
  in
  let kind =
    match MembershipExpr.f_op membership_expr with
    | `OpIn _ ->
        IR.Expr.In
    | `OpNotIn _ ->
        NotIn
  in
  let choices =
    MembershipExpr.f_membership_exprs membership_expr
    |> ExprAlternativesList.f_list
    |> List.map ~f:translate_membership_choice
  in
  let typ = translate_type_of_expr global_ctx (membership_expr :> Expr.t) in
  let expr_node = IR.Expr.Membership (prefix_expr, kind, choices) in
  with_data membership_expr typ expr_node


and translate_base_aggregate (base_aggregate : BaseAggregate.t) =
  let typ = translate_type_of_expr global_ctx (base_aggregate :> Expr.t) in
  match%nolazy base_aggregate with
  | #NullRecordAggregate.t ->
      with_data base_aggregate typ IR.Expr.NullRecordAggregate
  | `Aggregate {f_assocs= Some assoc_list} -> (
    match typ.desc with
    | Record _ ->
        let expr_node = translate_record_aggregate assoc_list in
        with_data base_aggregate typ expr_node
    | Array _ ->
        let expr_node = translate_array_aggregate assoc_list in
        with_data base_aggregate typ expr_node
    | _ ->
        Utils.legality_error
          "Expecting an array or record type for aggregate %a" Utils.pp_node
          base_aggregate )
  | _ ->
      Utils.legality_error "Expecting an assoc list for aggregate %a"
        Utils.pp_node base_aggregate


and translate_record_aggregate (assoc_list : AssocList.t) =
  let record_association {ParamActual.param; actual} =
    match (param, actual) with
    | Some param, Some actual ->
        let field = {IR.Expr.fieldname= param} in
        let expr =
          match actual with
          | #BoxExpr.t ->
              IR.Expr.Default
          | _ ->
              Expr (translate_expr actual)
        in
        {IR.Expr.field; expr}
    | _ ->
        Utils.lal_error "Cannot find a param or actual for %a" Utils.pp_node
          assoc_list
  in
  let assoc_with_params = AssocList.p_zip_with_params assoc_list in
  IR.Expr.RecordAggregate (List.map ~f:record_association assoc_with_params)


and translate_array_aggregate (assoc_list : AssocList.t) =
  let to_aggregate_assoc = function
    | #AggregateAssoc.t as assoc ->
        assoc
    | assoc ->
        Utils.legality_error "Expecting an aggregate association, found %a"
          Utils.pp_node assoc
  in
  let aggregate_assocs =
    List.map ~f:to_aggregate_assoc (AssocList.f_list assoc_list)
  in
  let translate_aggregate translate_assoc aggregate_assoc aggregate =
    (* Translate one association for either named or positional array
       aggregate, depending on the translation function given *)
    let designators =
      Option.value_map ~f:AlternativesList.f_list ~default:[]
        (AggregateAssoc.f_designators aggregate_assoc)
    in
    let expr =
      translate_expr (AggregateAssoc.f_r_expr aggregate_assoc :> Expr.t)
    in
    match%nolazy designators with
    | [#OthersDesignator.t] ->
        IR.Expr.{aggregate with others= Some expr}
    | designators ->
        let new_assoc = translate_assoc designators expr in
        {aggregate with assoc= new_assoc :: aggregate.assoc}
  in
  let init = IR.Expr.{assoc= []; others= None} in
  match%nolazy aggregate_assocs with
  | `AggregateAssoc {f_designators= Some (`AlternativesList {list= []})} :: _
  | `AggregateAssoc {f_designators= None} :: _ ->
      (* Positional array aggregate *)
      let aggregate =
        List.fold_right
          ~f:(translate_aggregate translate_positional_array_assoc)
          ~init aggregate_assocs
      in
      PositionalArrayAggregate aggregate
  | _ ->
      (* Named array aggregate *)
      let aggregate =
        List.fold_right
          ~f:(translate_aggregate translate_named_array_assoc)
          ~init aggregate_assocs
      in
      NamedArrayAggregate aggregate


and translate_positional_array_assoc (designators : AdaNode.t list)
    (expr : IR.Expr.t) =
  match%nolazy designators with
  | [] ->
      expr
  | _ ->
      (* not a positional array aggregate *)
      Utils.legality_error "Expecting a positional array aggregate"


and translate_named_array_assoc (designators : AdaNode.t list)
    (expr : IR.Expr.t) =
  match designators with
  | [] ->
      (* not a named array aggregate *)
      Utils.legality_error "Expecting a named array aggregate"
  | alternatives ->
      { IR.Expr.index= List.map ~f:translate_discrete_choice alternatives
      ; aggregate_expr= expr }


and translate_name (name : Name.t) : IR.Expr.t =
  match name with
  | #Lal_typ.identifier as ident when Lal_typ.is_variable ident ->
      let var = translate_variable ident in
      {var with node= Name var}
  | #DottedName.t as dotted_name when Lal_typ.is_record_access dotted_name ->
      let record_access = translate_record_access dotted_name in
      {record_access with node= Name record_access}
  | #Lal_typ.literal as literal when Lal_typ.is_literal literal ->
      translate_literal literal
  | #Lal_typ.call as call when Lal_typ.is_call call ->
      let typ = translate_type_of_expr global_ctx call in
      let function_call =
        with_data call typ
          (IR.Expr.FunctionCall (translate_call call, no_offset call typ))
      in
      {function_call with node= Name function_call}
  | #AttributeRef.t as attribute_ref ->
      translate_attribute_ref attribute_ref
  | #ExplicitDeref.t as explicit_deref -> (
      let prefix = (ExplicitDeref.f_prefix explicit_deref :> Expr.t) in
      let prefix_expr = translate_expr prefix in
      match name_from_expr false prefix_expr with
      | Some name ->
          let typ =
            translate_type_of_expr global_ctx (explicit_deref :> Expr.t)
          in
          let lval =
            with_data explicit_deref typ
              IR.Expr.(Lval (Mem name, no_offset explicit_deref typ))
          in
          {lval with node= Name lval}
      | None ->
          Utils.legality_error "Cannot deref a non lvalue: %a" IR.Expr.pp
            prefix_expr )
  | #CallExpr.t as call_expr ->
      (* Should not be a call *)
      let name = translate_call_expr call_expr in
      {name with node= Name name}
  | #QualExpr.t as qual_expr ->
      let typ = translate_type_of_expr global_ctx qual_expr in
      let qual_expr_node = translate_qual_expr qual_expr in
      let name =
        let offset = no_offset qual_expr typ in
        with_data qual_expr typ (IR.Expr.QualExpr (qual_expr_node, offset))
      in
      {name with node= Name name}
  | _ ->
      Utils.legality_error "Unexpected %a" Utils.pp_node name


and translate_literal (literal : Lal_typ.literal) =
  let open IR.Expr in
  let typ = translate_type_of_expr global_ctx (literal :> Expr.t) in
  match%nolazy literal with
  | #IntLiteral.t as int_literal ->
      try_or_undefined "IntLiteral.p_denoted_value"
        (fun n ->
          Const (Int (IR.Int_lit.of_int (IntLiteral.p_denoted_value n))) )
        int_literal typ
  | #StringLiteral.t as string_literal ->
      try_or_undefined "StringLiteral.p_denoted_value"
        (fun n -> Const (String (StringLiteral.p_denoted_value n)))
        string_literal typ
  | #NullLiteral.t ->
      with_data literal typ (Const Null)
  | #CharLiteral.t as char_lit ->
      (* A char literal is a regular enum. Use p_eval_as_int which correctly
         evaluates the position of the enum *)
      try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          let name = AdaNode.text n in
          Const
            (Enum
               { IR.Enum.name= StdCharLiteral name
               ; pos= IR.Int_lit.of_int (Expr.p_eval_as_int n) }) )
        char_lit typ
  | #RealLiteral.t as real_literal ->
      (* Not implemented *)
      unimplemented real_literal typ
  | #Lal_typ.identifier as ident ->
      (* Assume it denotes an enum. Otherwise, translate_literal should not
         have been called *)
      assert (Lal_typ.is_literal ident) ;
      let name = Utils.defining_name ident in
      try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          Const
            (Enum
               { IR.Enum.name= EnumLiteral name
               ; pos= IR.Int_lit.of_int (Expr.p_eval_as_int n) }) )
        ident typ


and translate_call (call : Lal_typ.call) =
  let add_self self subp_spec param_actuals =
    (* Assuming we are facing a dot call, add self to the param actuals with
       the good identifier *)
    match%nolazy BaseSubpSpec.p_params subp_spec with
    | `ParamSpec {f_ids= `DefiningNameList {list= first_id :: _}} :: _ ->
        {ParamActual.param= Some first_id; actual= (Some self :> Expr.t option)}
        :: param_actuals
    | _ ->
        Utils.legality_error
          "%a should have at least one parameter for dot call" Utils.pp_node
          subp_spec
  in
  let name_from_expr expr =
    (* locally reimplement name_from_expr to return a name and raise a
       legality rule *)
    match name_from_expr false expr with
    | Some name ->
        name
    | None ->
        Utils.legality_error "Cannot deref a non lvalue: %a" IR.Expr.pp expr
  in
  match%nolazy call with
  | `DottedName {f_prefix} as ident when Name.p_is_dot_call call ->
      (* Call to const function with self as arg *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec (add_self f_prefix subp_spec []) in
      (IR.Expr.Cfun (funinfo subp_spec), args)
  | #Lal_typ.identifier as ident ->
      (* Call to const function with no args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec [] in
      (Cfun (funinfo subp_spec), args)
  | `CallExpr
      {f_name= `DottedName {f_prefix} as ident; f_suffix= #AssocList.t as args}
    when Lal_typ.is_subprogram ident && Name.p_is_dot_call ident ->
      (* Const dot call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let param_actuals =
        add_self f_prefix subp_spec (AssocList.p_zip_with_params args)
      in
      let args = translate_args subp_spec param_actuals in
      (Cfun (funinfo subp_spec), args)
  | `CallExpr
      {f_name= #Lal_typ.identifier as ident; f_suffix= #AssocList.t as args}
    when Lal_typ.is_subprogram ident ->
      (* Const call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      (Cfun (funinfo subp_spec), args)
  | `ExplicitDeref {f_prefix} ->
      (* Call to non const function with no args *)
      let expr = translate_expr (f_prefix :> Expr.t) in
      (Pfun (name_from_expr expr), [])
  | `CallExpr
      { f_name= `ExplicitDeref {f_prefix= called} | called
      ; f_suffix= #AssocList.t as args } ->
      (* Call to non const function with args and explicit deref,
         or implicit *)
      let subp_spec = Utils.accessed_subp_spec called in
      let expr = translate_expr (called :> Expr.t) in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      (Pfun (name_from_expr expr), args)
  | `CallExpr {f_suffix} ->
      Utils.legality_error "Args should be an AssocList, found %a"
        Utils.pp_node f_suffix


and translate_args (subp_spec : BaseSubpSpec.t)
    (param_actuals : ParamActual.t list) =
  let module DefiningNameMap = Caml.Map.Make (DefiningName) in
  let rec as_lval_or_cast expr =
    match expr.IR.Expr.node with
    | IR.Expr.Name {node= Lval lval} ->
        `Lval lval
    | Name {node= Cast ((typ, e), {node= NoOffset})} ->
        `Cast (typ, as_lval_or_cast e)
    | _ ->
        Utils.legality_error "Expected a variable, found %a" Utils.pp_node
          expr.orig_node
  in
  let param_mode mode e =
    match mode with
    | IR.Expr.ModeIn ->
        IR.Expr.InParam e
    | ModeOut ->
        OutParam {e with node= as_lval_or_cast e}
    | ModeInOut ->
        InOutParam {e with node= as_lval_or_cast e}
  in
  (* First gather all ids of params in the right order *)
  let params = BaseSubpSpec.p_params subp_spec in
  let formals_with_default =
    let prepend_ids param_mode default_expr ids current_formals =
      List.fold_right
        ~f:(fun id acc -> (id, param_mode, default_expr) :: acc)
        ~init:current_formals ids
    in
    let prepend_param param current_formals =
      let default_expr = (ParamSpec.f_default_expr param :> Expr.t option) in
      let param_mode = convert_mode (ParamSpec.f_mode param) in
      let ids = DefiningNameList.f_list (ParamSpec.f_ids param) in
      prepend_ids param_mode default_expr ids current_formals
    in
    List.fold_right ~f:prepend_param ~init:[] params
  in
  (* Then fill the map with each position *)
  let pos_map =
    List.foldi
      ~f:(fun i map (id, _, _) -> DefiningNameMap.add id i map)
      ~init:DefiningNameMap.empty formals_with_default
  in
  let find_pos id =
    match DefiningNameMap.find_opt id pos_map with
    | Some pos ->
        pos
    | None ->
        Utils.lal_error "Cannot find parameter %a in subprogram spec %a"
          Utils.pp_node id Utils.pp_node subp_spec
  in
  (* Then sort the given param_actual according the the pos map *)
  let actuals =
    let compare {ParamActual.param= lparam} {ParamActual.param= rparam} =
      match (lparam, rparam) with
      | Some lparam, Some rparam ->
          Pervasives.compare (find_pos lparam) (find_pos rparam)
      | _, _ ->
          (* Should not happen, the type in lal is not precise enough *)
          Utils.lal_error "Not able to find a parameter"
    in
    List.sort ~compare param_actuals
  in
  (* sorted_param_actuals is now sorted according to the positions of
     params *)
  if List.length formals_with_default < List.length actuals then
    Utils.lal_error
      "Number of parameters between actuals and formals differ for %a"
      Utils.pp_node subp_spec ;
  let rec build_args formals actuals =
    match (formals, actuals) with
    | ( (id_formal, mode, _) :: formals
      , {ParamActual.param= Some id_actual; actual= Some actual} :: actuals )
      when DefiningName.equal id_formal id_actual ->
        (* id_formal and id_actual are the same, translate the actual
           expression *)
        param_mode mode (translate_expr actual) :: build_args formals actuals
    | (_, mode, Some formal) :: formals, actuals ->
        (* Since actuals are sorted according to params, if id_actual and
           id_formal are different, we should use a default expression *)
        param_mode mode (translate_expr formal) :: build_args formals actuals
    | (id_formal, _, None) :: _, _ ->
        Utils.legality_error "No default expr for parameter %a" Utils.pp_node
          id_formal
    | [], {ParamActual.param= Some id_actual} :: _ ->
        Utils.legality_error
          "Actual %a does not have a corresponding parameter" Utils.pp_node
          id_actual
    | [], [] ->
        (* No args *)
        []
    | _ ->
        (* Should not happen, the type in lal is not precise enough *)
        Utils.lal_error "Not able to find a parameter"
  in
  build_args formals_with_default actuals


and translate_type_or_expr (expr : Expr.t) : IR.Expr.type_or_expr =
  match expr with
  | #Lal_typ.identifier as ident -> (
    match try Name.p_name_designated_type ident with _ -> None with
    | Some typ ->
        `Type (translate_type_decl global_ctx typ)
    | None ->
        (* a simple expression in this case *)
        `Expr (translate_expr expr) )
  | _ ->
      `Expr (translate_expr expr)


and translate_type_or_name (implicit_deref : bool) (expr : Expr.t) =
  (* Translate the given expr as a type or a name *)
  let name_from_expr expr =
    match name_from_expr implicit_deref expr with
    | Some name ->
        name
    | None ->
        Utils.legality_error "Expecting a name, got %a" IR.Expr.pp expr
  in
  match translate_type_or_expr expr with
  | `Type typ ->
      `Type typ
  | `Expr e ->
      `Name (name_from_expr e)


and translate_fun_or_lval (implicit_deref : bool) (expr : Expr.t) =
  (* Translate the given expr as a subprogram or a name *)
  let lval_from_expr expr =
    match name_from_expr implicit_deref expr with
    | Some {node= Lval lval} ->
        lval
    | _ ->
        Utils.legality_error "Expecting a variable, got %a" IR.Expr.pp expr
  in
  match referenced_funinfo expr with
  | Some info ->
      `Fun info
  | None ->
      `Lval (lval_from_expr (translate_expr expr))


and translate_attribute_ref (attribute_ref : AttributeRef.t) =
  let typ = translate_type_of_expr global_ctx (attribute_ref :> Expr.t) in
  let attribute = Utils.attribute (AttributeRef.f_attribute attribute_ref) in
  let prefix = (AttributeRef.f_prefix attribute_ref :> Expr.t) in
  match attribute with
  | (`Access | `Unchecked_Access | `Unrestricted_Access | `Address) as
    access_kind ->
      let access_kind =
        match access_kind with
        | `Access ->
            IR.Expr.Access
        | `Unchecked_Access ->
            Unchecked_Access
        | `Unrestricted_Access ->
            Unrestricted_Access
        | `Address ->
            Address
      in
      let access =
        with_data attribute_ref typ
          (IR.Expr.AccessOf (access_kind, translate_fun_or_lval false prefix))
      in
      {access with node= Name access}
  | (`First | `Last | `Length) as attribute ->
      let prefix = translate_type_or_name true prefix in
      let index_opt =
        Option.map ~f:translate_arg_as_int (AttributeRef.f_args attribute_ref)
      in
      let expr_node =
        match attribute with
        | `First ->
            IR.Expr.AttributeRef (First (prefix, index_opt))
        | `Last ->
            AttributeRef (Last (prefix, index_opt))
        | `Length ->
            AttributeRef (Length (prefix, index_opt))
      in
      with_data attribute_ref typ expr_node
  | `Result -> (
    match referenced_funinfo prefix with
    | Some info ->
        with_data attribute_ref typ (IR.Expr.AttributeRef (Result info))
    | None ->
        Utils.legality_error "Expecting a function, got a %a" Utils.pp_node
          prefix )
  | _ ->
      unimplemented attribute_ref typ


and translate_call_expr (call_expr : CallExpr.t) =
  (* Handle call expr other than subprogram call *)
  let name = CallExpr.f_name call_expr in
  match try Name.p_name_designated_type name with _ -> None with
  | Some lal_typ -> (
      (* This is a cast *)
      match%nolazy CallExpr.f_suffix call_expr with
      | `AssocList {list= [`ParamAssoc {f_r_expr}]} ->
          let typ = translate_type_decl global_ctx lal_typ in
          let suffix_expr = translate_expr (f_r_expr :> Expr.t) in
          with_data call_expr typ
            (IR.Expr.Cast ((typ, suffix_expr), no_offset call_expr typ))
      | suffix ->
          Utils.legality_error
            "Expect an AssocList with one element for a type cast, found %a"
            Utils.pp_node suffix )
  | None ->
      (* Either an index access or a slice, in each case, we can translate
         the name as an expression *)
      let name_expr = translate_expr (name :> Expr.t) in
      let name =
        match name_from_expr true name_expr with
        | Some name ->
            name
        | None ->
            Utils.legality_error "Cannot access an index of a non lvalue: %a"
              IR.Expr.pp name_expr
      in
      if CallExpr.p_is_array_slice call_expr then
        translate_array_slice name call_expr
      else translate_array_index name call_expr


and translate_array_index name call_expr =
  (* Regular array access *)
  let typ = translate_type_of_expr global_ctx (call_expr :> Expr.t) in
  let suffix = CallExpr.f_suffix call_expr in
  match%nolazy suffix with
  | `AssocList {list= assoc_list} -> (
      (* translate each index *)
      let translate_assoc =
        function%nolazy
        | `ParamAssoc {ParamAssoc.f_r_expr} ->
            translate_expr (f_r_expr :> Expr.t)
        | assoc ->
            Utils.legality_error
              "Expect a ParamAssoc with index expression, found %a"
              Utils.pp_node assoc
      in
      let index = List.map ~f:translate_assoc assoc_list in
      let f offset = with_data call_expr typ (IR.Expr.Index (offset, index)) in
      match append_offset ~f name.node with
      | Some name ->
          name
      | None ->
          Utils.legality_error "Cannot get the index for %a" Utils.pp_node
            (CallExpr.f_name call_expr) )
  | _ ->
      Utils.legality_error
        "Expect an AssocList with one element for a type cast, found %a"
        Utils.pp_node suffix


and translate_array_slice name call_expr =
  (* Array slicing *)
  let typ = translate_type_of_expr global_ctx (call_expr :> Expr.t) in
  let suffix = CallExpr.f_suffix call_expr in
  match%nolazy suffix with
  | `AssocList {list= [`ParamAssoc {f_r_expr}]} -> (
    match (f_r_expr :> AdaNode.t) with
    | #Lal_typ.discrete_range as range -> (
        let f offset =
          with_data call_expr typ
            (IR.Expr.Slice (offset, translate_discrete_range range))
        in
        match append_offset ~f name.node with
        | Some name ->
            name
        | None ->
            Utils.legality_error "Cannot get a slice for %a" Utils.pp_node
              (CallExpr.f_name call_expr) )
    | _ ->
        Utils.legality_error "Expect an range for an array slice, found %a"
          Utils.pp_node f_r_expr )
  | #Lal_typ.discrete_range as range -> (
      (* All possibilities does not translate to an assoc list. For example,
         a DiscreteSubtypeIndication is directly here instead of under an
         assoc list *)
      let f offset =
        with_data call_expr typ
          (IR.Expr.Slice (offset, translate_discrete_range range))
      in
      match append_offset ~f name.node with
      | Some name ->
          name
      | None ->
          Utils.legality_error "Cannot get a slice for %a" Utils.pp_node
            (CallExpr.f_name call_expr) )
  | _ ->
      Utils.legality_error "Expect an range for an array slice, found %a"
        Utils.pp_node suffix


and translate_discrete_range (range : Lal_typ.discrete_range) :
    IR.Expr.discrete_range =
  let to_discrete_type typ =
    match typ.IR.Typ.desc with
    | Discrete discrete_type ->
        `DiscreteType discrete_type
    | _ ->
        Utils.legality_error "Expected a discrete type in range %a"
          Utils.pp_node range
  in
  match%nolazy range with
  | #Lal_typ.identifier as ident -> (
    (* The only way to translate a range from an identifier, is if the
       identifier refers to a type *)
    match translate_type_or_expr (ident :> Expr.t) with
    | `Type typ ->
        to_discrete_type typ
    | _ ->
        Utils.legality_error "Expect a type for range %a" Utils.pp_node ident )
  | #DiscreteSubtypeIndication.t as type_expr ->
      (* Explicit discrete subtype indication. We translate the underlying type
         expression and see if it's a range constraint *)
      to_discrete_type
        (translate_type_expr global_ctx (type_expr :> TypeExpr.t))
  | #Lal_typ.range as range ->
      `Range (translate_range range)


and translate_arg_as_int (args : AdaNode.t) =
  (* Compute the index of the given args *)
  match%nolazy args with
  | `AssocList {list= [`ParamAssoc {f_r_expr}]} ->
      let index =
        try Expr.p_eval_as_int f_r_expr
        with _ ->
          Utils.legality_error "Expect a static expression, got %a"
            Utils.pp_node f_r_expr
      in
      index
  | arg ->
      Utils.legality_error
        "Expect an AssocList with one element for a range attribute, found %a"
        Utils.pp_node arg


and translate_range (range : Lal_typ.range) : IR.Expr.range =
  match%nolazy range with
  | `BinOp {f_left; f_op= `OpDoubleDot _; f_right} ->
      (* DoubleDot here *)
      DoubleDot
        (translate_expr (f_left :> Expr.t), translate_expr (f_right :> Expr.t))
  | #BinOp.t as binop ->
      Utils.legality_error "Expect double dot operator for a range, got %a"
        Utils.pp_node binop
  | #AttributeRef.t as attribute_ref -> (
    match Utils.attribute (AttributeRef.f_attribute attribute_ref) with
    | `Range ->
        let index_opt =
          Option.map ~f:translate_arg_as_int
            (AttributeRef.f_args attribute_ref)
        in
        let prefix =
          translate_type_or_name true
            (AttributeRef.f_prefix attribute_ref :> Expr.t)
        in
        Range (prefix, index_opt)
    | _ ->
        Utils.legality_error "Expect range_attribute_ref for a range, got %a"
          Utils.pp_node attribute_ref )


and translate_qual_expr (qual_expr : QualExpr.t) : IR.Expr.qual_expr =
  let prefix = QualExpr.f_prefix qual_expr in
  let subtype_mark =
    match try Name.p_name_designated_type prefix with _ -> None with
    | Some typ ->
        translate_type_decl global_ctx typ
    | None ->
        Utils.legality_error
          "Expect a subtype mark for prefix of qualified expression, found %a"
          Utils.pp_node prefix
  in
  let suffix = translate_expr (QualExpr.f_suffix qual_expr :> Expr.t) in
  (subtype_mark, suffix)


and translate_box_expr (_box_expr : BoxExpr.t) = assert false

and translate_if_expr (if_expr : IfExpr.t) =
  let typ = translate_type_of_expr global_ctx (if_expr :> Expr.t) in
  let translate_else_part = function
    | Some expr ->
        translate_expr (expr :> Expr.t)
    | None ->
        (* Should be true expression here *)
        assert false
  in
  let translate_alternative alternative else_expr =
    let cond_expr =
      translate_expr (ElsifExprPart.f_cond_expr alternative :> Expr.t)
    in
    let then_expr =
      translate_expr (ElsifExprPart.f_then_expr alternative :> Expr.t)
    in
    let if_node = IR.Expr.If (cond_expr, then_expr, else_expr) in
    with_data if_expr typ if_node
  in
  let cond_expr = translate_expr (IfExpr.f_cond_expr if_expr :> Expr.t) in
  let then_expr = translate_expr (IfExpr.f_then_expr if_expr :> Expr.t) in
  let else_expr = translate_else_part (IfExpr.f_else_expr if_expr) in
  let alternatives =
    ElsifExprPartList.f_list (IfExpr.f_alternatives if_expr)
  in
  let elsif_expr =
    List.fold_right ~f:translate_alternative ~init:else_expr alternatives
  in
  let if_node = IR.Expr.If (cond_expr, then_expr, elsif_expr) in
  with_data if_expr typ if_node


and translate_discrete_choice (node : AdaNode.t) =
  match node with
  | #Lal_typ.discrete_range as discrete_range
    when Lal_typ.is_discrete_range discrete_range ->
      (translate_discrete_range discrete_range :> IR.Expr.discrete_choice)
  | #Expr.t as expr ->
      `Expr (translate_expr (expr :> Expr.t))
  | #OthersDesignator.t ->
      Utils.legality_error
        "others should appear alone and be the last alternative of the \
         aggregate"
  | _ ->
      Utils.lal_error "Unexpected node %a for discrete_choice" Utils.pp_node
        node


and translate_case_expr (case_expr : CaseExpr.t) =
  let translate_alternative (alternatives, others) alternative =
    let when_expr =
      translate_expr (CaseExprAlternative.f_expr alternative :> Expr.t)
    in
    match
      AlternativesList.f_list (CaseExprAlternative.f_choices alternative)
    with
    | [#OthersDesignator.t] ->
        (alternatives, Some when_expr)
    | choices ->
        let choices = List.map ~f:translate_discrete_choice choices in
        ({IR.Expr.choices; when_expr} :: alternatives, others)
  in
  let expr = translate_expr (CaseExpr.f_expr case_expr :> Expr.t) in
  let alternatives, others =
    List.fold ~f:translate_alternative ~init:([], None)
      (CaseExprAlternativeList.f_list (CaseExpr.f_cases case_expr))
  in
  let typ = translate_type_of_expr global_ctx (case_expr :> Expr.t) in
  let case_node = IR.Expr.Case (expr, List.rev alternatives, others) in
  with_data case_expr typ case_node


and translate_case_expr_alternative
    (_case_expr_alternative : CaseExprAlternative.t) =
  assert false


and translate_quantified_expr (quantified_expr : QuantifiedExpr.t) =
  let quantifier =
    match QuantifiedExpr.f_quantifier quantified_expr with
    | #QuantifierAll.t ->
        IR.Expr.ForAll
    | #QuantifierSome.t ->
        Exists
  in
  let iterator_spec =
    translate_iterator_specification
      (QuantifiedExpr.f_loop_spec quantified_expr)
  in
  let predicate =
    translate_expr (QuantifiedExpr.f_expr quantified_expr :> Expr.t)
  in
  let typ = translate_type_of_expr global_ctx (quantified_expr :> Expr.t) in
  let quantified_node =
    IR.Expr.Quantified (quantifier, iterator_spec, predicate)
  in
  with_data quantified_expr typ quantified_node


and translate_iterator_specification (loop_spec : ForLoopSpec.t) =
  let name_from_expr expr =
    (* reimplement this locally to directy raise legality error *)
    match name_from_expr false expr with
    | Some e ->
        e
    | None ->
        Utils.legality_error "Expected a name for loop iterator, got %a"
          IR.Expr.pp expr
  in
  let var_name = ForLoopVarDecl.f_id (ForLoopSpec.f_var_decl loop_spec) in
  let reversed =
    match ForLoopSpec.f_has_reverse loop_spec with
    | #ReversePresent.t ->
        true
    | #ReverseAbsent.t ->
        false
  in
  let iter_kind =
    match ForLoopSpec.f_loop_type loop_spec with
    | #IterTypeIn.t -> (
      match ForLoopSpec.f_iter_expr loop_spec with
      | #Lal_typ.discrete_range as range when Lal_typ.is_discrete_range range
        ->
          IR.Expr.Iterator (translate_discrete_range range)
      | expr ->
          Utils.legality_error "Expecting a discrete range, got %a"
            Utils.pp_node expr )
    | #IterTypeOf.t -> (
      match
        ( ForLoopSpec.f_iter_expr loop_spec
          :> [DiscreteSubtypeIndication.t | Expr.t] )
      with
      | #Expr.t as expr ->
          Iterable (name_from_expr (translate_expr expr))
      | #DiscreteSubtypeIndication.t as typ ->
          Utils.legality_error "Unexpected subtype indication, got %a"
            Utils.pp_node typ )
  in
  IR.Expr.{var_name; reversed; iter_kind}


and translate_allocator (allocator : Allocator.t) =
  let typ = translate_type_of_expr global_ctx (allocator :> Expr.t) in
  let allocator_node =
    match Allocator.f_type_or_expr allocator with
    | #SubtypeIndication.t as subtype_indication ->
        let type_expr =
          translate_type_expr global_ctx (subtype_indication :> TypeExpr.t)
        in
        IR.Expr.Allocator (type_expr, None)
    | #QualExpr.t as qual_expr ->
        let qual_typ, expr = translate_qual_expr qual_expr in
        IR.Expr.Allocator (qual_typ, Some expr)
  in
  with_data allocator typ allocator_node


and translate_raise_expr (raise_expr : RaiseExpr.t) =
  let name =
    match RaiseExpr.f_exception_name raise_expr with
    | Some (#Lal_typ.identifier as ident) ->
        Utils.defining_name ident
    | Some expr ->
        Utils.legality_error
          "Expected an identifier for the exception name, found %a"
          Utils.pp_node expr
    | None ->
        Utils.legality_error "No identifier given for exception expression"
          Utils.pp_node raise_expr
  in
  let msg =
    Option.map
      ~f:(fun e -> translate_expr (e :> Expr.t))
      (RaiseExpr.f_error_message raise_expr)
  in
  let typ = translate_type_of_expr global_ctx (raise_expr :> Expr.t) in
  let raise_node = IR.Expr.Raise (name, msg) in
  with_data raise_expr typ raise_node


(* Start of type translation *)
and translate_bound e =
  let expr = translate_expr e in
  match expr.node with Const c -> IR.Typ.Static c | _ -> Dynamic expr


and translate_custom_type ctx base_type_decl =
  (* handle translation of some custom types specially. Return an optional
     type. [Some typ] means that [typ] is the custom translation of the type.
     If [None], the given type is not a type with a special representation. *)
  let name =
    try BasicDecl.p_unique_identifying_name base_type_decl with _ -> ""
  in
  let orig_node = base_type_decl in
  match name with
  | "standard.universal_int_type_" ->
      (* Special case for this type, return int64 type annotated with
         UniversalInteger name *)
      Some
        { IR.Typ.desc= int64_desc
        ; aspects= []
        ; name= UniversalInteger
        ; parent_type= None
        ; orig_node }
  | "standard.character" ->
      Some
        { desc= char_desc
        ; aspects= []
        ; name= StdCharacter
        ; parent_type= None
        ; orig_node }
  | "standard.wide_character" ->
      Some
        { desc= wide_char_desc
        ; aspects= []
        ; name= StdWideCharacter
        ; parent_type= None
        ; orig_node }
  | "standard.wide_wide_character" ->
      Some
        { desc= wide_wide_char_desc
        ; aspects= []
        ; name= StdWideWideCharacter
        ; parent_type= None
        ; orig_node }
  | "system.address" ->
      (* Annotate the type with Address name *)
      Some {(translate_type_decl_impl ctx base_type_decl) with name= Address}
  | _ ->
      None


and translate_type_decl_impl ctx base_type_decl =
  let typ =
    match%nolazy (base_type_decl :> BaseTypeDecl.t) with
    | #TypeDecl.t as type_decl ->
        (* This is a type declaration, the type depends on the type definition *)
        translate_type_def ctx type_decl (TypeDecl.f_type_def type_decl)
    | `SubtypeDecl {f_subtype} ->
        (* For a subtype, we can simply fallback on calling the translation for
           a type expr *)
        translate_subtype_indication ctx f_subtype
    | #IncompleteTypeDecl.t as incomplete -> (
      match BaseTypeDecl.p_next_part base_type_decl with
      | Some type_decl ->
          translate_type_decl ctx type_decl
      | None ->
          (* Did not found next part, try to find the complete type calling
             complete_type_decl *)
          Utils.lal_error "Cannot find complete type decl for %a."
            Utils.pp_node incomplete )
    | #DiscreteBaseSubtypeDecl.t ->
        (* TODO: we do not yet compute base types, return 64 bit int here
           instead *)
        mk base_type_decl int64_desc
    | #ClasswideTypeDeclType.t -> (
      match AdaNode.parent base_type_decl with
      | Some (#BaseTypeDecl.t as base_type) ->
          translate_type_decl ctx base_type
      | Some n ->
          Utils.lal_error "For %a, found parent %a, expecting a BaseTypeDecl"
            Utils.pp_node base_type_decl Utils.pp_node n
      | None ->
          Utils.lal_error "Cannot find parent for %a" Utils.pp_node
            base_type_decl )
    | #TaskTypeDeclType.t | #ProtectedTypeDeclType.t ->
        mk base_type_decl int64_desc
  in
  let aspects = get_aspects base_type_decl in
  {(mk base_type_decl typ.desc) with aspects= merge_aspects typ.aspects aspects}


and translate_constraint _tenv _orig_type _constr = assert false

and translate_subtype_indication ctx subtype_indication =
  let orig_type = get_orig_type ctx (subtype_indication :> TypeExpr.t) in
  let new_type =
    match SubtypeIndication.f_constraint subtype_indication with
    | Some constr ->
        (* Add the constraints to the type *)
        translate_constraint ctx orig_type constr
    | None ->
        orig_type
  in
  {new_type with parent_type= Some orig_type}


and get_orig_type ctx type_expr =
  let open Option.Monad_infix in
  match
    TypeExpr.p_designated_type_decl type_expr >>| translate_type_decl ctx
  with
  | Some orig_type ->
      orig_type
  | None ->
      Utils.lal_error "Cannot get the orig type for type expr %a" Utils.pp_node
        type_expr


and translate_type_def ctx type_decl type_def =
  match%nolazy (type_def :> TypeDef.t) with
  | #EnumTypeDef.t as enum_type_def ->
      translate_enum_type_def type_decl enum_type_def
  | #SignedIntTypeDef.t as signed_int_type_def ->
      translate_signed_int_type_def type_decl signed_int_type_def
  | #ModIntTypeDef.t as mod_type_def ->
      translate_mod_type_def type_decl mod_type_def
  | #RecordTypeDef.t as record_type_def ->
      translate_record_type_def ctx type_decl record_type_def
  | #RealTypeDef.t as real_type_def ->
      translate_real_type_def type_decl real_type_def
  | `AnonymousTypeAccessDef {f_type_decl} ->
      (* anonymous access *)
      let root_typ = translate_type_decl ctx f_type_decl in
      mk type_decl (Access (AccessKind, Object root_typ))
  | `TypeAccessDef {f_subtype_indication} ->
      (* access definition *)
      let root_typ =
        translate_type_expr ctx (f_subtype_indication :> TypeExpr.t)
      in
      mk type_decl (Access (AccessKind, Object root_typ))
  | #DerivedTypeDef.t as derived_type_def ->
      translate_derived_type_def ctx type_decl derived_type_def
  | #ArrayTypeDef.t as array_type_def ->
      (* type Arr is array (1 .. 10) of Integer *)
      translate_array_type_def ctx type_decl array_type_def
  | #PrivateTypeDef.t
  | `DerivedTypeDef {f_has_with_private= `WithPrivatePresent _} -> (
    match BaseTypeDecl.p_next_part type_decl with
    | Some type_decl ->
        !trans_type_decl tenv type_decl
    | None ->
        lal_error "Cannot find next part for %s"
          (AdaNode.short_image type_decl) )
  | `AccessToSubpDef {f_subp_spec= `SubpSpec {f_subp_returns}} ->
      let subp_returns = (f_subp_returns :> TypeExpr.t option) in
      let return_typ =
        Option.value_map ~default:void ~f:(!trans_type_expr tenv) subp_returns
      in
      let subprogram = mk (Subprogram return_typ) in
      mk (Access subprogram)
  | #InterfaceTypeDef.t ->
      (* Simply translate an interface type into an empty record *)
      let name = get_type_name type_decl in
      ( match lookup tenv name with
      | None ->
          TypenameHash.add tenv name {discriminants= []; fields= []}
      | Some _ ->
          () ) ;
      mk (Record (name, Unconstrained))
  | #FormalDiscreteTypeDef.t as type_def ->
      unimplemented "trans_type_decl for %s" (AdaNode.short_image type_def)


and translate_enum_type_def type_decl enum_type_def =
  (* example of an enum type definition is:
         type Color is (Red, Green, Blue)

     first value for enum types is 0 and last is (length - 1)

     Red is 0
     Green is 1
     Blue is 2 *)
  let enum_decl_list =
    EnumTypeDef.f_enum_literals enum_type_def |> EnumLiteralDeclList.f_list
  in
  let to_enum_name enum_literal_decl =
    IR.Enum.EnumLiteral (EnumLiteralDecl.f_name enum_literal_decl)
  in
  let name_list = List.map ~f:to_enum_name enum_decl_list in
  mk type_decl (Discrete (Enum (OtherEnum name_list), None))


and translate_signed_int_type_def type_decl signed_int_type_def =
  (* Implement signed_integer_type_definition (Ada RM 3.5.4) *)
  let range_spec = SignedIntTypeDef.f_range signed_int_type_def in
  match%nolazy RangeSpec.f_range range_spec with
  | `BinOp {f_left; f_op= `OpDoubleDot _; f_right} ->
      let left = to_int (translate_expr (f_left :> Expr.t)) in
      let right = to_int (translate_expr (f_right :> Expr.t)) in
      mk type_decl (Discrete (Signed (left, right), None))
  | _ as op ->
      Utils.legality_error "Expecting range x .. y, found %a" Utils.pp_node op


and translate_mod_type_def type_decl mod_type_def =
  (* Implement modular_type_definition (Ada RM 3.5.4) *)
  let modulus_expr = (ModIntTypeDef.f_expr mod_type_def :> Expr.t) in
  let modulus = to_int (translate_expr modulus_expr) in
  mk type_decl (Discrete (Modular modulus, None))


and translate_discriminants ctx type_decl =
  let translate_discriminant discriminant =
    (* A discriminant spec possibly contains multiple ids, but they all
       share the same type, so map them to the name and the type of
       the discriminant *)
    let discr_typ =
      let type_expr =
        (DiscriminantSpec.f_type_expr discriminant :> TypeExpr.t)
      in
      let typ = translate_type_expr ctx type_expr in
      match typ.desc with
      | Discrete discrete ->
          `Discrete discrete
      | Access (AccessKind, root_typ) ->
          `Access root_typ
      | _ ->
          Utils.legality_error
            "Type of discriminant should be a discrete type or anonymous \
             access type, found %a"
            Utils.pp_node type_expr
    in
    DiscriminantSpec.f_ids discriminant
    |> DefiningNameList.f_list
    |> List.map ~f:(fun discr_name -> {IR.Typ.discr_name; discr_typ})
  in
  let discriminants =
    match%nolazy TypeDecl.f_discriminants type_decl with
    | Some
        (`KnownDiscriminantPart
          {f_discr_specs= `DiscriminantSpecList {list= discriminants}}) ->
        discriminants
    | _ ->
        []
  in
  List.concat_map ~f:translate_discriminant discriminants


and translate_discriminant_constraints type_decl =
  let discriminants =
    match%nolazy TypeDecl.f_discriminants type_decl with
    | Some
        (`KnownDiscriminantPart
          {f_discr_specs= `DiscriminantSpecList {list= discriminants}}) ->
        discriminants
    | _ ->
        []
  in
  match discriminants with
  | [] ->
      (* No discriminants, thus this is a constrained record *)
      IR.Typ.RecConstrained []
  | h :: _ -> (
      (* The head determines weather this is a constrained record or not *)
      let constrained discriminant =
        match DiscriminantSpec.f_default_expr discriminant with
        | Some default_expr ->
            let expr = translate_expr (default_expr :> Expr.t) in
            DiscriminantSpec.f_ids discriminant
            |> DefiningNameList.f_list
            |> List.map ~f:(fun discr_name -> (discr_name, expr))
        | None ->
            Utils.legality_error
              "No expression for discriminant %a in constrained record"
              Utils.pp_node discriminant
      in
      match DiscriminantSpec.f_default_expr h with
      | Some _ ->
          RecConstrained (List.concat_map ~f:constrained discriminants)
      | None ->
          (* Unconstrained record *)
          RecUnconstrained )


(** Translate a record definition into a list of fields. *)
and translate_fields ctx base_record_def =
  let translate_component field_constraint component =
    (* This function translates one component to a list of fields with the
       given field constraint *)
    match%nolazy (component :> AdaNode.t) with
    | `ComponentDecl
        { f_ids= `DefiningNameList {list= ids}
        ; f_component_def= `ComponentDef {f_type_expr} } ->
        let field_typ = translate_type_expr ctx (f_type_expr :> TypeExpr.t) in
        List.map
          ~f:(fun name ->
            {IR.Typ.field_name= name; field_typ; field_constraint} )
          ids
    | _ ->
        (* We can have pragmas or null here, but we ignore them in this
           function *)
        []
  in
  let rec translate_variant_part (field_constraint : IR.Typ.field_constraint) =
    (* This function translates one variant part with the give field
       constraint. The field constraint is the constraint from the top of the
       type to the point where appears the variant part. Variant part can be
       nested, this is why we need to keep the field constraint *)
    function%nolazy
    | `VariantPart
        {VariantPartType.f_discr_name; f_variant= `VariantList {list= variants}}
      ->
        (* A variant part is of the form:
           case f_discr_name is
             when choices =>
               component_list

           A variant part is a case .. is .. end case;' block in a
           discriminated record declaration. It contains a list of
           variants that each corresponds to a "when choices => components"
           section of the case.

           for each variant, construct the new condition based on the given one
           and the alternative_list, and call trans_component with this
           newly created condition on the component_list *)
        let discriminant =
          match Name.p_referenced_defining_name f_discr_name with
          | Some discr ->
              discr
          | None ->
              Utils.lal_error "Cannot get the discriminant %a" Utils.pp_node
                f_discr_name
        in
        let translate_variant (not_alternatives, fields) variant =
          (* This function is used to fold each variant in the variant part
             the accumulator contains both the alternatives that we need to
             negate to come to this variant and the fields contains the
             current fields that we already translated *)
          let translate_choice choice =
            (* create a condition for this choice *)
            match choice with
            | #OthersDesignator.t ->
                (* Simply ignore the others constraint *)
                None
            | _ ->
                let discrete_choice = translate_discrete_choice choice in
                Some discrete_choice
          in
          let choices =
            List.filter_map ~f:translate_choice
              (Variant.f_choices variant |> AlternativesList.f_list)
          in
          let alternatives = {IR.Typ.discriminant; choices} in
          (* For the next variants, this variant should be negated, this is
             why we add it to not_alternatives. For the fields, we recursively
             call translate_component_list with the new alternative added to
             the field constraint *)
          ( alternatives :: not_alternatives
          , fields
            @ translate_component_list
                { IR.Typ.not_alternatives
                ; alternatives= alternatives :: field_constraint.alternatives
                }
                (Variant.f_components variant) )
        in
        (* We do not need the last negated alternatives since there is no more
           fields to translate *)
        let _, result =
          List.fold_left ~f:translate_variant
            ~init:(field_constraint.not_alternatives, [])
            variants
        in
        result
  and translate_component_list field_constraint component_list =
    (* Translate a list of component to a list of field by constructing the
       field constraint *)
    let open Option.Monad_infix in
    let component_fields =
      let components =
        (* components, simply default it to [] if there is no components since
           a variant part with no components is possible *)
        ComponentList.f_components component_list
        >>| AdaNodeList.f_list |> Option.value ~default:[]
      in
      List.concat_map ~f:(translate_component field_constraint) components
    in
    let variant_part_fields =
      let variant_part =
        (* variant part, simply default it to [] if there is no variant part
           since components with no variants is possible *)
        ComponentList.f_variant_part component_list
      in
      Option.value ~default:[]
        (variant_part >>| translate_variant_part field_constraint)
    in
    (* Append both fields from components and fields from variant part *)
    component_fields @ variant_part_fields
  in
  let component_list = BaseRecordDef.f_components base_record_def in
  translate_component_list
    {not_alternatives= []; alternatives= []}
    component_list


and translate_record_type_def ctx type_decl record_type_def =
  (* record definition *)
  let name = Utils.decl_defining_name type_decl in
  ( match IR.Typ.record_opt ctx.tenv name with
  | Some _ ->
      (* The record is already registered *)
      ()
  | None when IR.Name.Set.mem name ctx.translating ->
      (* The record is being translated *)
      ()
  | None ->
      (* First insert a dummy record description, to avoid infinite
         recursion with recursive structures *)
      let new_ctx =
        {ctx with translating= IR.Name.Set.add name ctx.translating}
      in
      let discriminants = translate_discriminants new_ctx type_decl in
      let base_record_def = RecordTypeDef.f_record_def record_type_def in
      let fields = translate_fields new_ctx base_record_def in
      IR.Typ.add_record ctx.tenv name {discriminants; fields} ) ;
  let record_constraint = translate_discriminant_constraints type_decl in
  mk type_decl (Record (name, record_constraint))


and translate_real_type_def type_decl _real_type_def =
  (* TODO: simple float for now without taking ranges in account *)
  mk type_decl (Real Float)


and translate_derived_type_def ctx type_decl derived_type_def =
  (* type Derived is new SomeType *)
  let subtype_indication =
    DerivedTypeDef.f_subtype_indication derived_type_def
  in
  let derived_from_type =
    translate_subtype_indication ctx subtype_indication
  in
  match derived_from_type.desc with
  | Record (derived_rec_name, _) ->
      (* TODO the derived discriminant constraint is not taken into account.
         if there is one, it should bind the discriminants of this type, to
         the discriminants of the derived type *)
      let record_extension =
        DerivedTypeDef.f_record_extension derived_type_def
      in
      let name = Utils.decl_defining_name type_decl in
      (* If the type we derive from is a record, check for any record
         extension. *)
      ( match IR.Typ.record_opt ctx.tenv name with
      | Some _ ->
          ()
      | None when IR.Name.Set.mem name ctx.translating ->
          (* The record is being translated *)
          ()
      | None ->
          (* First insert a dummy record description, to avoid infinite
             recursion with recursive structures *)
          IR.Typ.add_record ctx.tenv name {discriminants= []; fields= []} ;
          let derived_record = IR.Typ.record ctx.tenv derived_rec_name in
          let discriminants =
            match translate_discriminants ctx type_decl with
            | [] ->
                (* No discriminants, they are inherited from the parent, see
                   Ada RM 3.4 (11) *)
                derived_record.discriminants
            | discriminants ->
                (* Discriminants are defined by this type. *)
                discriminants
          in
          let new_record =
            match record_extension with
            | Some record_def ->
                let fields = translate_fields ctx record_def in
                {IR.Typ.discriminants; fields= fields @ derived_record.fields}
            | None ->
                {discriminants; fields= derived_record.fields}
          in
          IR.Typ.add_record ctx.tenv name new_record ) ;
      let record_constraint = translate_discriminant_constraints type_decl in
      mk type_decl (Record (name, record_constraint))
  | _ ->
      derived_from_type


and translate_array_type_def ctx type_decl =
function%nolazy
  | `ArrayTypeDef {f_indices; f_component_type= `ComponentDef {f_type_expr}} -> (
    let name = Utils.decl_defining_name type_decl in
    match IR.Typ.elt_typ_opt ctx.tenv name with
    | Some _ ->
        (* elt_type is already known *)
        ()
    | None when Name.Set.mem name ctx.translating ->
        (* type is being translated, avoid recursion *)
        ()
    | None ->
      let new_ctx = {ctx with translating= Name.Set.add name ctx.translating} in
      let elt_typ = translate_type_expr new_ctx (f_type_expr :> TypeExpr.t) in
      let base_constraint =
        match%nolazy f_indices with
        | `ConstrainedArrayIndices {f_list= `ConstraintList {list= constraints}}
          ->
            (* For constrained array indices, we fallback on calling
             * the translation of a list constraint *)
            trans_array_constraints ctx constraints
        | `UnconstrainedArrayIndices _ as unconstrained ->
            trans_unconstrained_array ctx unconstrained
      in
      Array (name, elt_typ, base_constraint)

and translate_type_decl ctx base_type_decl =
  (* Mnemoize this function in tenv *)
  match IR.Typ.find_base_type_decl ctx.tenv base_type_decl with
  | Some typ ->
      typ
  | None ->
      let result =
        match translate_custom_type ctx base_type_decl with
        | Some typ ->
            typ
        | None ->
            translate_type_decl_impl ctx base_type_decl
      in
      IR.Typ.add_base_type_decl ctx.tenv base_type_decl result ;
      result


and translate_type_expr ctx (type_expr : TypeExpr.t) : IR.Typ.t =
  match type_expr with
  | #SubtypeIndication.t as subtype_indication ->
      translate_subtype_indication ctx subtype_indication
  | _ -> (
    match TypeExpr.p_designated_type_decl type_expr with
    | Some type_decl ->
        translate_type_decl ctx type_decl
    | None ->
        Utils.lal_error "Cannot find designated type for %a" Utils.pp_node
          type_expr )


and translate_type_of_expr (ctx : type_context) (expr : [< Expr.t]) : IR.Typ.t
    =
  match Expr.p_expression_type expr with
  | Some base_type_decl ->
      translate_type_decl ctx base_type_decl
  | None ->
      Utils.lal_error "cannot find type for expression %s"
        (AdaNode.short_image expr)


let translate_expr (expr : [< Expr.t]) = translate_expr (expr :> Expr.t)
