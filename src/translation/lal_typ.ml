open Libadalang

type identifier = [DottedName.t | Identifier.t]

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

type call = [CallExpr.t | DottedName.t | Identifier.t | ExplicitDeref.t]

type subprogram_decl =
  [BasicSubpDecl.t | GenericSubpInstantiation.t | GenericDecl.t]

let is_literal = function
  | #IntLiteral.t
  | #StringLiteral.t
  | #NullLiteral.t
  | #CharLiteral.t
  | #RealLiteral.t ->
      true
  | #identifier as ident -> (
    try
      match Name.p_referenced_decl ident with
      | Some #EnumLiteralDecl.t ->
          true
      | Some _ ->
          false
      | None ->
          Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
          false
    with PropertyError s ->
      Utils.log_warning "PropertyError %s for %a calling p_referenced_decl" s
        Utils.pp_node ident ;
      false )

let is_variable ident =
  try
    match Name.p_referenced_decl ident with
    | Some #variable_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
        false
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a calling p_referenced_decl" s
      Utils.pp_node ident ;
    false

let is_record_access dotted_name =
  try
    match Name.p_referenced_decl dotted_name with
    | Some #field_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node dotted_name ;
        false
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a calling p_referenced_decl" s
      Utils.pp_node dotted_name ;
    false

let is_lvalue name =
  match name with
  | #Identifier.t as ident ->
      is_variable ident
  | #DottedName.t as dotted_name ->
      is_variable dotted_name || is_record_access dotted_name
  | _ ->
      (* Always return false otherwise for now *)
      false

let is_subprogram ident =
  try
    match Name.p_referenced_decl ident with
    | Some #subprogram_decl ->
        true
    | Some _ ->
        false
    | None ->
        Utils.log_warning "No declaration for %a" Utils.pp_node ident ;
        false
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a calling p_referenced_decl" s
      Utils.pp_node ident ;
    false

let is_call (call : call) =
  try
    match%nolazy call with
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
  with PropertyError s ->
    Utils.log_warning "PropertyError %s for %a" s Utils.pp_node call ;
    false

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
