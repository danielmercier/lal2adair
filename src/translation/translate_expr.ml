open Libadalang

(** translate an expression to a lvalue checking for an implicit deref *)
let name_from_expr (check_implicit : bool) (expr : IR.Expr.t) : IR.Expr.name option =
  match expr.node with
  | Name name ->
      if check_implicit && Lal_typ.is_access_type expr.typ then
        (* Implicit deref *)
        Some (Deref name)
      else Some name
  | _ ->
      None


let funinfo (spec : BaseSubpSpec.t) =
  match%nolazy spec with
  | `SubpSpec {f_subp_name= Some fname} ->
      IR.Expr.{fname}
  | _ ->
      Utils.lal_error "Cannot find name for subprogram spec %a" Utils.pp_node spec


let referenced_funinfo (expr : Expr.t) =
  match expr with
  | #Lal_typ.identifier as ident when Lal_typ.is_subprogram ident ->
      let subp_spec = Utils.referenced_subp_spec ident in
      Some (funinfo subp_spec)
  | _ ->
      None

let fieldinfo (ident : Lal_typ.identifier) : IR.Expr.fieldinfo =
  {fieldname= Utils.defining_name ident}


let rec translate_expr (expr : Expr.t) : IR.Expr.t =
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


and translate_variable (var : Lal_typ.identifier) =
  let vname = Utils.defining_name var in
  IR.Expr.Var (Source {vname})


and translate_record_access (name : DottedName.t) : IR.Expr.name =
  match DottedName.f_suffix name with
  | #Identifier.t as ident ->
      let prefix = translate_expr (DottedName.f_prefix name :> Expr.t) in
      let name =
        match name_from_expr true prefix with
        | Some lval ->
            lval
        | None ->
            Utils.legality_error "Cannot access a field of a non lvalue: %a" IR.Expr.pp prefix
      in
      Field (name, fieldinfo ident)
  | suffix ->
      (* For a record access, the only possible suffix is an identifier *)
      Utils.lal_error "Expecting an identifier for field, found %a" Utils.pp_node suffix


and translate_contract_cases (_contract_cases : ContractCases.t) = assert false

and translate_unop (unop : UnOp.t) =
  (* First check if there is a user defined operator *)
  let op = UnOp.f_op unop in
  let expr = translate_expr (UnOp.f_expr unop :> Expr.t) in
  match try Name.p_referenced_decl op with _ -> None with
  | Some _ ->
      let subp_spec = Utils.referenced_subp_spec op in
      Name (FunctionCall (Cfun (funinfo subp_spec), [expr]))
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
      Unop (operator, expr)


and translate_binop (binop : BinOp.t) =
  let op = BinOp.f_op binop in
  let lexpr = translate_expr (BinOp.f_left binop :> Expr.t) in
  let rexpr = translate_expr (BinOp.f_right binop :> Expr.t) in
  match try Name.p_referenced_decl op with _ -> None with
  | Some _ ->
      let subp_spec = Utils.referenced_subp_spec op in
      Name (FunctionCall (Cfun (funinfo subp_spec), [lexpr; rexpr]))
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
            Utils.legality_error "Unexpected binary operator %a" Utils.pp_node op
      in
      Binop (operator, lexpr, rexpr)


and translate_membership_expr (membership_expr : MembershipExpr.t) : IR.Expr.expr_node =
  let translate_membership_choice expr =
    match expr with
    | #Lal_typ.range as range when Lal_typ.is_range range ->
        `Range (translate_range range)
    | _ ->
        (translate_type_or_expr (expr :> Expr.t) :> IR.Expr.membership_choice)
  in
  let prefix_expr = translate_expr (MembershipExpr.f_expr membership_expr :> Expr.t) in
  let kind =
    match MembershipExpr.f_op membership_expr with `OpIn _ -> IR.Expr.In | `OpNotIn _ -> NotIn
  in
  let choices =
    MembershipExpr.f_membership_exprs membership_expr
    |> ExprAlternativesList.f_list
    |> List.map ~f:translate_membership_choice
  in
  Membership (prefix_expr, kind, choices)


and translate_base_aggregate (base_aggregate : BaseAggregate.t) =
  match%nolazy base_aggregate with
  | #NullRecordAggregate.t ->
      IR.Expr.NullRecordAggregate
  | `Aggregate {f_assocs= Some assoc_list} ->
      let typ = Translate_typ.translate_type_of_expr base_aggregate in
      if BaseTypeDecl.p_is_record_type typ then translate_record_aggregate assoc_list
      else if BaseTypeDecl.p_is_array_type typ then translate_array_aggregate assoc_list
      else
        Utils.legality_error "Expecting an array or record type for aggregate %a" Utils.pp_node
          base_aggregate
  | _ ->
      Utils.legality_error "Expecting an assoc list for aggregate %a" Utils.pp_node base_aggregate


and translate_record_aggregate (assoc_list : AssocList.t) =
  let record_association {ParamActual.param; actual} =
    match (param, actual) with
    | Some param, Some actual ->
        let field = {IR.Expr.fieldname= param} in
        let expr =
          match actual with #BoxExpr.t -> IR.Expr.Default | _ -> Expr (translate_expr actual)
        in
        {IR.Expr.field; expr}
    | _ ->
        Utils.lal_error "Cannot find a param or actual for %a" Utils.pp_node assoc_list
  in
  let assoc_with_params = AssocList.p_zip_with_params assoc_list in
  IR.Expr.RecordAggregate (List.map ~f:record_association assoc_with_params)


and translate_array_aggregate (assoc_list : AssocList.t) =
  let to_aggregate_assoc = function
    | #AggregateAssoc.t as assoc ->
        assoc
    | assoc ->
        Utils.legality_error "Expecting an aggregate association, found %a" Utils.pp_node assoc
  in
  let aggregate_assocs = List.map ~f:to_aggregate_assoc (AssocList.f_list assoc_list) in
  let translate_aggregate translate_assoc aggregate_assoc aggregate =
    (* Translate one association for either named or positional array
       aggregate, depending on the translation function given *)
    let designators =
      Option.value_map ~f:AlternativesList.f_list ~default:[]
        (AggregateAssoc.f_designators aggregate_assoc)
    in
    let expr = translate_expr (AggregateAssoc.f_r_expr aggregate_assoc :> Expr.t) in
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
        List.fold_right ~f:(translate_aggregate translate_named_array_assoc) ~init aggregate_assocs
      in
      NamedArrayAggregate aggregate


and translate_positional_array_assoc (designators : AdaNode.t list) (expr : IR.Expr.t) =
  match%nolazy designators with
  | [] ->
      expr
  | _ ->
      (* not a positional array aggregate *)
      Utils.legality_error "Expecting a positional array aggregate"


and translate_named_array_assoc (designators : AdaNode.t list) (expr : IR.Expr.t) =
  match designators with
  | [] ->
      (* not a named array aggregate *)
      Utils.legality_error "Expecting a named array aggregate"
  | alternatives ->
      {IR.Expr.index= List.map ~f:translate_discrete_choice alternatives; aggregate_expr= expr}


and translate_name (name : Name.t) : IR.Expr.expr_node =
  match name with
  | #Lal_typ.identifier as ident when Lal_typ.is_variable ident ->
      Name (translate_variable ident)
  | #DottedName.t as dotted_name when Lal_typ.is_record_access dotted_name ->
      Name (translate_record_access dotted_name)
  | #Lal_typ.literal as literal when Lal_typ.is_literal literal ->
      translate_literal literal
  | #Lal_typ.call as call when Lal_typ.is_call call ->
      Name (translate_call call)
  | #AttributeRef.t as attribute_ref ->
      Name (translate_attribute_ref attribute_ref)
  | #ExplicitDeref.t as explicit_deref -> (
      let prefix = (ExplicitDeref.f_prefix explicit_deref :> Expr.t) in
      let prefix_expr = translate_expr prefix in
      match name_from_expr false prefix_expr with
      | Some name ->
          Name (Deref name)
      | None ->
          Utils.legality_error "Cannot deref a non lvalue: %a" IR.Expr.pp prefix_expr )
  | #CallExpr.t as call_expr ->
      (* Should not be a call *)
      Name (translate_call_expr call_expr)
  | #QualExpr.t as qual_expr ->
      Name (translate_qual_expr qual_expr)
  | _ ->
      Utils.legality_error "Unexpected %a" Utils.pp_node name


and translate_literal (literal : Lal_typ.literal) =
  let open IR.Expr in
  match%nolazy literal with
  | #IntLiteral.t as int_literal ->
      Utils.try_or_undefined "IntLiteral.p_denoted_value"
        (fun n -> Const (Int (IR.Int_lit.of_int (IntLiteral.p_denoted_value n))))
        int_literal
  | #StringLiteral.t as string_literal ->
      Utils.try_or_undefined "StringLiteral.p_denoted_value"
        (fun n -> Const (String (StringLiteral.p_denoted_value n)))
        string_literal
  | #NullLiteral.t ->
      Const Null
  | #CharLiteral.t as char_lit ->
      (* A char literal is a regular enum. Use p_eval_as_int which correctly
         evaluates the position of the enum *)
      Utils.try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          let name = AdaNode.text n in
          Name
            (Enum {IR.Enum.name= StdCharLiteral name; pos= IR.Int_lit.of_int (Expr.p_eval_as_int n)})
          )
        char_lit
  | #RealLiteral.t as real_literal ->
      (* Not implemented *)
      Name (Utils.unimplemented real_literal)
  | #Lal_typ.identifier as ident ->
      (* Assume it denotes an enum. Otherwise, translate_literal should not
         have been called *)
      assert (Lal_typ.is_literal ident) ;
      let name = Utils.defining_name ident in
      Utils.try_or_undefined "Expr.p_eval_as_int"
        (fun n ->
          Name
            (Enum {IR.Enum.name= EnumLiteral name; pos= IR.Int_lit.of_int (Expr.p_eval_as_int n)})
          )
        ident


and translate_call (call : Lal_typ.call) =
  let add_self self subp_spec param_actuals =
    (* Assuming we are facing a dot call, add self to the param actuals with
       the good identifier *)
    match%nolazy BaseSubpSpec.p_params subp_spec with
    | `ParamSpec {f_ids= `DefiningNameList {list= first_id :: _}} :: _ ->
        {ParamActual.param= Some first_id; actual= (Some self :> Expr.t option)} :: param_actuals
    | _ ->
        Utils.legality_error "%a should have at least one parameter for dot call" Utils.pp_node
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
      IR.Expr.FunctionCall (Cfun (funinfo subp_spec), args)
  | #Lal_typ.identifier as ident ->
      (* Call to const function with no args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec [] in
      FunctionCall (Cfun (funinfo subp_spec), args)
  | `CallExpr {f_name= `DottedName {f_prefix} as ident; f_suffix= #AssocList.t as args}
    when Lal_typ.is_subprogram ident && Name.p_is_dot_call ident ->
      (* Const dot call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let param_actuals = add_self f_prefix subp_spec (AssocList.p_zip_with_params args) in
      let args = translate_args subp_spec param_actuals in
      FunctionCall (Cfun (funinfo subp_spec), args)
  | `CallExpr {f_name= #Lal_typ.identifier as ident; f_suffix= #AssocList.t as args}
    when Lal_typ.is_subprogram ident ->
      (* Const call with args *)
      let subp_spec = Utils.referenced_subp_spec ident in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      FunctionCall (Cfun (funinfo subp_spec), args)
  | `ExplicitDeref {f_prefix} ->
      (* Call to non const function with no args *)
      let expr = translate_expr (f_prefix :> Expr.t) in
      FunctionCall (Pfun (name_from_expr expr), [])
  | `CallExpr {f_name= `ExplicitDeref {f_prefix= called} | called; f_suffix= #AssocList.t as args}
    ->
      (* Call to non const function with args and explicit deref,
         or implicit *)
      let subp_spec = Utils.accessed_subp_spec called in
      let expr = translate_expr (called :> Expr.t) in
      let args = translate_args subp_spec (AssocList.p_zip_with_params args) in
      FunctionCall (Pfun (name_from_expr expr), args)
  | `CallExpr {f_suffix} ->
      Utils.legality_error "Args should be an AssocList, found %a" Utils.pp_node f_suffix


and translate_args (subp_spec : BaseSubpSpec.t) (param_actuals : ParamActual.t list) =
  let module DefiningNameMap = Caml.Map.Make (DefiningName) in
  (* First gather all ids of params in the right order *)
  let params = BaseSubpSpec.p_params subp_spec in
  let formals_with_default =
    let prepend_ids default_expr ids current_formals =
      List.fold_right ~f:(fun id acc -> (id, default_expr) :: acc) ~init:current_formals ids
    in
    let prepend_param param current_formals =
      let default_expr = (ParamSpec.f_default_expr param :> Expr.t option) in
      let ids = DefiningNameList.f_list (ParamSpec.f_ids param) in
      prepend_ids default_expr ids current_formals
    in
    List.fold_right ~f:prepend_param ~init:[] params
  in
  (* Then fill the map with each position *)
  let pos_map =
    List.foldi
      ~f:(fun i map (id, _) -> DefiningNameMap.add id i map)
      ~init:DefiningNameMap.empty formals_with_default
  in
  let find_pos id =
    match DefiningNameMap.find_opt id pos_map with
    | Some pos ->
        pos
    | None ->
        Utils.lal_error "Cannot find parameter %a in subprogram spec %a" Utils.pp_node id
          Utils.pp_node subp_spec
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
    Utils.lal_error "Number of parameters between actuals and formals differ for %a" Utils.pp_node
      subp_spec ;
  let rec build_args formals actuals =
    match (formals, actuals) with
    | ( (id_formal, _) :: formals
      , {ParamActual.param= Some id_actual; actual= Some actual} :: actuals )
      when DefiningName.equal id_formal id_actual ->
        (* id_formal and id_actual are the same, translate the actual
           expression *)
        translate_expr actual :: build_args formals actuals
    | (_, Some formal) :: formals, actuals ->
        (* Since actuals are sorted according to params, if id_actual and
           id_formal are different, we should use a default expression *)
        translate_expr formal :: build_args formals actuals
    | (id_formal, None) :: _, _ ->
        Utils.legality_error "No default expr for parameter %a" Utils.pp_node id_formal
    | [], {ParamActual.param= Some id_actual} :: _ ->
        Utils.legality_error "Actual %a does not have a corresponding parameter" Utils.pp_node
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
        `Type typ
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


and translate_fun_or_name (implicit_deref : bool) (expr : Expr.t) =
  (* Translate the given expr as a subprogram or a name *)
  let name_from_expr expr =
    match name_from_expr implicit_deref expr with
    | Some name ->
        name
    | None ->
        Utils.legality_error "Expecting a name, got %a" IR.Expr.pp expr
  in
  match referenced_funinfo expr with
  | Some info -> `Fun info
  | None -> `Name (name_from_expr (translate_expr expr))


and translate_attribute_ref (attribute_ref : AttributeRef.t) =
  let attribute = Utils.attribute (AttributeRef.f_attribute attribute_ref) in
  let prefix = (AttributeRef.f_prefix attribute_ref :> Expr.t) in
  match attribute with
  | `Access ->
      IR.Expr.AttributeRef (Access (translate_fun_or_name false prefix))
  | `Unchecked_Access ->
      AttributeRef (Unchecked_Access (translate_fun_or_name false prefix))
  | `Unrestricted_Access ->
      AttributeRef (Unrestricted_Access (translate_fun_or_name false prefix))
  | `Address ->
      AttributeRef (Address (translate_fun_or_name false prefix))
  | (`First | `Last | `Length) as attribute -> (
      let prefix = translate_type_or_name true prefix in
      let index_opt = Option.map ~f:translate_arg_as_int (AttributeRef.f_args attribute_ref) in
      match attribute with
      | `First ->
          AttributeRef (First (prefix, index_opt))
      | `Last ->
          AttributeRef (Last (prefix, index_opt))
      | `Length ->
          AttributeRef (Length (prefix, index_opt)) )
  | `Result -> (
      match referenced_funinfo prefix with
      | Some info -> AttributeRef (Result (info))
      | None ->
         Utils.legality_error "Expecting a function, got a %a" Utils.pp_node prefix)
  | _ ->
      Utils.unimplemented attribute_ref


and translate_call_expr (call_expr : CallExpr.t) : IR.Expr.name =
  (* Handle call expr other than subprogram call *)
  let name = CallExpr.f_name call_expr in
  match try Name.p_name_designated_type name with _ -> None with
  | Some typ -> (
      (* This is a cast *)
      match%nolazy CallExpr.f_suffix call_expr with
      | `AssocList {list= [`ParamAssoc {f_r_expr}]} ->
          let suffix_expr = translate_expr (f_r_expr :> Expr.t) in
          Cast (typ, suffix_expr)
      | suffix ->
          Utils.legality_error "Expect an AssocList with one element for a type cast, found %a"
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
            Utils.legality_error "Cannot access an index of a non lvalue: %a" IR.Expr.pp name_expr
      in
      let suffix = CallExpr.f_suffix call_expr in
      if CallExpr.p_is_array_slice call_expr then translate_array_slice name suffix
      else translate_array_index name suffix


and translate_array_index name suffix =
  (* Regular array access *)
  match%nolazy suffix with
  | `AssocList {list= assoc_list} ->
      (* translate each index *)
      let translate_assoc =
        function%nolazy
        | `ParamAssoc {ParamAssoc.f_r_expr} ->
            translate_expr (f_r_expr :> Expr.t)
        | assoc ->
            Utils.legality_error "Expect a ParamAssoc with index expression, found %a"
              Utils.pp_node assoc
      in
      let index = List.map ~f:translate_assoc assoc_list in
      Index (name, index)
  | _ ->
      Utils.legality_error "Expect an AssocList with one element for a type cast, found %a"
        Utils.pp_node suffix


and translate_array_slice name suffix =
  (* Array slicing *)
  match%nolazy suffix with
  | `AssocList {list= [`ParamAssoc {f_r_expr}]} -> (
    match (f_r_expr :> AdaNode.t) with
    | #Lal_typ.discrete_range as range ->
        IR.Expr.Slice (name, translate_discrete_range range)
    | _ ->
        Utils.legality_error "Expect an range for an array slice, found %a" Utils.pp_node f_r_expr
    )
  | #Lal_typ.discrete_range as range ->
      (* All possibilities does not translate to an assoc list. For example,
         a DiscreteSubtypeIndication is directly here instead of under an
         assoc list *)
      Slice (name, translate_discrete_range range)
  | _ ->
      Utils.legality_error "Expect an range for an array slice, found %a" Utils.pp_node suffix


and translate_discrete_range (range : Lal_typ.discrete_range) : IR.Expr.discrete_range =
  match%nolazy range with
  | #Lal_typ.identifier as ident -> (
    (* The only way to translate a range from an identifier, is if the
       identifier refers to a type *)
    match translate_type_or_expr (ident :> Expr.t) with
    | `Type typ ->
        `DiscreteType (typ, None)
    | _ ->
        Utils.legality_error "Expect a type for range %a" Utils.pp_node ident )
  | #DiscreteSubtypeIndication.t as type_expr -> (
    (* Explicit discrete subtype indication. We translate the underlying type
       expression and see if it's a range constraint *)
    match translate_type_expr (type_expr :> TypeExpr.t) with
    | typ, Some (RangeConstraint (left, right)) ->
        `DiscreteType (typ, Some (left, right))
    | typ, None ->
        `DiscreteType (typ, None) )
  | #Lal_typ.range as range ->
      `Range (translate_range range)


and translate_arg_as_int (args : AdaNode.t) =
  (* Compute the index of the given args *)
  match%nolazy args with
  | `AssocList {list= [`ParamAssoc {f_r_expr}]} ->
      let index =
        try Expr.p_eval_as_int f_r_expr
        with _ ->
          Utils.legality_error "Expect a static expression, got %a" Utils.pp_node f_r_expr
      in
      index
  | arg ->
      Utils.legality_error "Expect an AssocList with one element for a range attribute, found %a"
        Utils.pp_node arg


and translate_range (range : Lal_typ.range) : IR.Expr.range =
  match%nolazy range with
  | `BinOp {f_left; f_op= `OpDoubleDot _; f_right} ->
      (* DoubleDot here *)
      DoubleDot (translate_expr (f_left :> Expr.t), translate_expr (f_right :> Expr.t))
  | #BinOp.t as binop ->
      Utils.legality_error "Expect double dot operator for a range, got %a" Utils.pp_node binop
  | #AttributeRef.t as attribute_ref -> (
    match Utils.attribute (AttributeRef.f_attribute attribute_ref) with
    | `Range ->
        let index_opt = Option.map ~f:translate_arg_as_int (AttributeRef.f_args attribute_ref) in
        let prefix = translate_type_or_name true (AttributeRef.f_prefix attribute_ref :> Expr.t) in
        Range (prefix, index_opt)
    | _ ->
        Utils.legality_error "Expect range_attribute_ref for a range, got %a" Utils.pp_node
          attribute_ref )


and translate_type_expr (type_expr : TypeExpr.t) : IR.Expr.type_expr =
  match TypeExpr.p_designated_type_decl type_expr with
  | Some typ -> (
    match type_expr with
    | #SubtypeIndication.t as subtype_indication -> (
        let constr = SubtypeIndication.f_constraint subtype_indication in
        match%nolazy constr with
        | Some (`RangeConstraint {f_range= `RangeSpec {f_range}}) -> (
            match%nolazy f_range with
            | `BinOp {f_left; f_op= `OpDoubleDot _; f_right} ->
                let left = translate_expr (f_left :> Expr.t) in
                let right = translate_expr (f_right :> Expr.t) in
                (typ, Some (RangeConstraint (left, right)))
            | _ ->
                Utils.legality_error "Expect a range, found %a" Utils.pp_node f_range )
        | _ ->
            (typ, None) )
    | _ ->
        (typ, None) )
  | None ->
      Utils.lal_error "Cannot find designated type for %a" Utils.pp_node type_expr


and translate_qual_expr (qual_expr : QualExpr.t) : IR.Expr.name =
  let prefix = QualExpr.f_prefix qual_expr in
  let subtype_mark =
    match try Name.p_name_designated_type prefix with _ -> None with
    | Some typ ->
        typ
    | None ->
        Utils.legality_error "Expect a subtype mark for prefix of qualified expression, found %a"
          Utils.pp_node prefix
  in
  let suffix = translate_expr (QualExpr.f_suffix qual_expr :> Expr.t) in
  QualExpr (subtype_mark, suffix)


and translate_box_expr (_box_expr : BoxExpr.t) = assert false

and translate_if_expr (if_expr : IfExpr.t) =
  let typ = Translate_typ.translate_type_of_expr if_expr in
  let translate_else_part = function
    | Some expr ->
        translate_expr (expr :> Expr.t)
    | None ->
        (* Should be true expression here *)
        assert false
  in
  let translate_alternative alternative else_expr =
    let cond_expr = translate_expr (ElsifExprPart.f_cond_expr alternative :> Expr.t) in
    let then_expr = translate_expr (ElsifExprPart.f_then_expr alternative :> Expr.t) in
    {IR.Expr.node= If (cond_expr, then_expr, else_expr); orig_node= (if_expr :> Expr.t); typ}
  in
  let cond_expr = translate_expr (IfExpr.f_cond_expr if_expr :> Expr.t) in
  let then_expr = translate_expr (IfExpr.f_then_expr if_expr :> Expr.t) in
  let else_expr = translate_else_part (IfExpr.f_else_expr if_expr) in
  let alternatives = ElsifExprPartList.f_list (IfExpr.f_alternatives if_expr) in
  let elsif_expr = List.fold_right ~f:translate_alternative ~init:else_expr alternatives in
  IR.Expr.If (cond_expr, then_expr, elsif_expr)


and translate_discrete_choice (node : AdaNode.t) =
  match node with
  | #Lal_typ.discrete_range as discrete_range when Lal_typ.is_discrete_range discrete_range ->
      (translate_discrete_range discrete_range :> IR.Expr.discrete_choice)
  | #Expr.t as expr ->
      `Expr (translate_expr (expr :> Expr.t))
  | #OthersDesignator.t ->
      Utils.legality_error
        "others should appear alone and be the last alternative of the aggregate"
  | _ ->
      Utils.lal_error "Unexpected node %a for discrete_choice" Utils.pp_node node


and translate_case_expr (case_expr : CaseExpr.t) =
  let translate_alternative (alternatives, others) alternative =
    let when_expr = translate_expr (CaseExprAlternative.f_expr alternative :> Expr.t) in
    match AlternativesList.f_list (CaseExprAlternative.f_choices alternative) with
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
  IR.Expr.Case (expr, List.rev alternatives, others)


and translate_case_expr_alternative (_case_expr_alternative : CaseExprAlternative.t) = assert false

and translate_quantified_expr (quantified_expr : QuantifiedExpr.t) =
  let quantifier =
    match QuantifiedExpr.f_quantifier quantified_expr with
    | #QuantifierAll.t ->
        IR.Expr.ForAll
    | #QuantifierSome.t ->
        Exists
  in
  let iterator_spec =
    translate_iterator_specification (QuantifiedExpr.f_loop_spec quantified_expr)
  in
  let predicate = translate_expr (QuantifiedExpr.f_expr quantified_expr :> Expr.t) in
  IR.Expr.Quantified (quantifier, iterator_spec, predicate)


and translate_iterator_specification (loop_spec : ForLoopSpec.t) =
  let name_from_expr expr =
    (* reimplement this locally to directy raise legality error *)
    match name_from_expr false expr with
    | Some e ->
        e
    | None ->
        Utils.legality_error "Expected a name for loop iterator, got %a" IR.Expr.pp expr
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
      | #Lal_typ.discrete_range as range when Lal_typ.is_discrete_range range ->
          IR.Expr.Iterator (translate_discrete_range range)
      | expr ->
          Utils.legality_error "Expecting a discrete range, got %a" Utils.pp_node expr )
    | #IterTypeOf.t -> (
      match (ForLoopSpec.f_iter_expr loop_spec :> [DiscreteSubtypeIndication.t | Expr.t]) with
      | #Expr.t as expr ->
          Iterable (name_from_expr (translate_expr expr))
      | #DiscreteSubtypeIndication.t as typ ->
          Utils.legality_error "Unexpected subtype indication, got %a" Utils.pp_node typ )
  in
  IR.Expr.{var_name; reversed; iter_kind}


and translate_allocator (allocator : Allocator.t) =
  match Allocator.f_type_or_expr allocator with
  | #SubtypeIndication.t as subtype_indication ->
      let type_expr = translate_type_expr (subtype_indication :> TypeExpr.t) in
      IR.Expr.Allocator (type_expr, None)
  | #QualExpr.t as qual_expr -> (
    match translate_qual_expr qual_expr with
    | QualExpr (typ, expr) ->
        Allocator ((typ, None), Some expr)
    | _ ->
        assert false )


and translate_raise_expr (raise_expr : RaiseExpr.t) =
  let name =
    match RaiseExpr.f_exception_name raise_expr with
    | Some (#Lal_typ.identifier as ident) ->
        Utils.defining_name ident
    | Some expr ->
        Utils.legality_error "Expected an identifier for the exception name, found %a"
          Utils.pp_node expr
    | None ->
        Utils.legality_error "No identifier given for exception expression" Utils.pp_node
          raise_expr
  in
  let msg =
    Option.map ~f:(fun e -> translate_expr (e :> Expr.t)) (RaiseExpr.f_error_message raise_expr)
  in
  IR.Expr.Raise (name, msg)


let translate_expr (expr : [< Expr.t]) = translate_expr (expr :> Expr.t)
