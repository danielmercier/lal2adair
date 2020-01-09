open Libadalang

let mk orig_node desc =
  {IR.Typ.desc; orig_node; name= Name (Utils.decl_defining_name orig_node)}

let int64_desc =
  let min_int = IR.Int_lit.of_string "-0x8000000000000000" in
  let max_int = IR.Int_lit.of_string "0x7FFFFFFFFFFFFFFF" in
  IR.Typ.Discrete (Signed (Static (Int min_int), Static (Int max_int)))


let char_desc =
  IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 256)))


let wide_char_desc =
  IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 65536)))


let wide_wide_char_desc =
  IR.Typ.Discrete (Enum (Character (IR.Int_lit.of_int 2147483648)))


let translate_custom_type tenv base_type_decl =
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
      Some {IR.Typ.desc= int64_desc; orig_node= base_type_decl; name= UniversalInteger}
  | "standard.character" ->
      Some {desc= char_desc base_type_decl; orig_no}
  | "standard.wide_character" ->
      Some (wide_char_typ base_type_decl)
  | "standard.wide_wide_character" ->
      Some (wide_wide_char_typ base_type_decl)
  | "system.address" -> (
        (* Annotate the type with Address name *)
        Some (translate_type_decl tenv)
  | _ ->
      None

and translate_type_decl tenv base_type_decl


let trans_type_decl tenv base_type_decl =
  match trans_custom_type base_type_decl with
  | Some typ ->
      typ
  | None ->
      let typ =
        match%nolazy (base_type_decl :> BaseTypeDecl.t) with
        | #TypeDecl.t as type_decl ->
            (* This is a type declaration, the type depends on the type definition *)
            trans_type_def tenv type_decl (TypeDecl.f_type_def type_decl)
        | `SubtypeDecl {f_subtype} ->
            (* For a subtype, we can simply fallback on calling the translation for
       * a type expr *)
            trans_subtype_indication tenv f_subtype
        | #IncompleteTypeDecl.t as incomplete -> (
          match BaseTypeDecl.p_next_part base_type_decl with
          | Some type_decl ->
              !trans_type_decl tenv type_decl
          | None ->
              (* Did not found next part, try to find the complete type calling
             * complete_type_decl *)
              lal_error "Cannot find complete type decl for %s."
                (AdaNode.short_image incomplete) )
        | #DiscreteBaseSubtypeDecl.t ->
            (* TODO: we do not yet compute base types, return 64 bit int here
           instead *)
            let min_int = IntLit.of_string "-0x8000000000000000" in
            let max_int = IntLit.of_string "0x7FFFFFFFFFFFFFFF" in
            mk (Discrete (Signed (Int min_int, Int max_int)))
        | #ClasswideTypeDeclType.t -> (
          match AdaNode.parent base_type_decl with
          | Some (#BaseTypeDecl.t as base_type) ->
              !trans_type_decl tenv base_type
          | Some n ->
              lal_error "For %s, found parent %s, expecting a BaseTypeDecl"
                (AdaNode.short_image base_type_decl)
                (AdaNode.short_image n)
          | None ->
              lal_error "Cannot find parent for %s"
                (AdaNode.short_image base_type_decl) )
        | #TaskTypeDeclType.t | #ProtectedTypeDeclType.t ->
            unimplemented "trans_type_decl for %s"
              (AdaNode.short_image base_type_decl)
      in
      let aspects = get_aspects base_type_decl in
      {desc= typ.desc; aspects= merge_aspects typ.aspects aspects}


let translate_type_of_expr (expr : [< Expr.t]) : IR.Typ.t =
  match Expr.p_expression_type expr with
  | Some base_type_decl ->
      (base_type_decl :> IR.Typ.t)
  | None ->
      Utils.lal_error "cannot find type for expression %s"
        (AdaNode.short_image expr)
