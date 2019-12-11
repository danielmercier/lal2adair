open Libadalang

type identifier = [DottedName.t | Identifier.t | StringLiteral.t]

type literal =
  [ IntLiteral.t
  | StringLiteral.t
  | NullLiteral.t
  | CharLiteral.t
  | RealLiteral.t
  | identifier ]

type variable_decl =
  [ ParamSpec.t
  | GenericFormal.t
  | NumberDecl.t
  | ObjectDecl.t
  | ForLoopVarDecl.t ]

type field_decl = [DiscriminantSpec.t | ComponentDecl.t]

type call = [identifier | CallExpr.t | ExplicitDeref.t]

type range = [BinOp.t | AttributeRef.t]

type discrete_range =
  [identifier | BinOp.t | AttributeRef.t | DiscreteSubtypeIndication.t]

type subprogram_decl =
  [ BasicSubpDecl.t
  | BaseSubpBody.t
  | GenericSubpInstantiation.t
  | GenericSubpDecl.t
  | SubpBodyStub.t ]

let map_property ~f ~default value =
  try f value
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a" s Utils.pp_node value ;
    default

let is_literal = function
  | #IntLiteral.t
  | #StringLiteral.t
  | #NullLiteral.t
  | #CharLiteral.t
  | #RealLiteral.t ->
      true
  | #identifier as ident ->
      let f ident =
        match Name.p_referenced_decl ident with
        | Some #EnumLiteralDecl.t ->
            true
        | Some _ ->
            false
        | None ->
            Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
            false
      in
      map_property ~f ~default:false ident

let is_variable ident =
  let f ident =
    match Name.p_referenced_decl ident with
    | Some #variable_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
        false
  in
  map_property ~f ~default:false (ident :> identifier)

let is_record_access dotted_name =
  let f dotted_name =
    match Name.p_referenced_decl dotted_name with
    | Some #field_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node dotted_name ;
        false
  in
  map_property ~f ~default:false dotted_name

let is_lvalue name =
  match (name :> Name.t) with
  | #Identifier.t as ident ->
      is_variable ident
  | #DottedName.t as dotted_name ->
      is_variable dotted_name || is_record_access dotted_name
  | _ ->
      (* Always return false otherwise for now *)
      false

let is_subprogram ident =
  let f ident =
    match Name.p_referenced_decl ident with
    | Some #subprogram_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
        false
  in
  map_property ~f ~default:false (ident :> identifier)

let is_call call =
  let f call =
    match%nolazy (call : call) with
    | #identifier as ident ->
        is_subprogram ident
    | `ExplicitDeref {f_prefix} -> (
      match Expr.p_expression_type f_prefix with
      | Some (#TypeDecl.t as type_decl) -> (
        match TypeDecl.f_type_def type_decl with
        | #AccessToSubpDef.t ->
            true
        | _ ->
            false )
      | Some _ ->
          false
      | None ->
          Utils.log_warning "Cannot find expression type for %a" Utils.pp_node
            call ;
          false )
    | #CallExpr.t ->
        Name.p_is_call call
  in
  map_property ~f ~default:false (call :> call)

let map_to_full_view ~default ~f typ =
  try
    let full_typ = Option.value ~default:typ (BaseTypeDecl.p_full_view typ) in
    f full_typ
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a" s Utils.pp_node typ ;
    default

let is_access_type typ =
  map_to_full_view ~f:BaseTypeDecl.p_is_access_type ~default:false
    (typ :> BaseTypeDecl.t)

let is_range range =
  match%nolazy (range :> range) with
  | `BinOp {f_op= `OpDoubleDot _} ->
      true
  | #BinOp.t ->
      false
  | #AttributeRef.t as attribute_ref -> (
    match Utils.attribute (AttributeRef.f_attribute attribute_ref) with
    | `Range ->
        true
    | _ ->
        false )
