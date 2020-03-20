open Libadalang

let ctx = AnalysisContext.create ()

let u = AnalysisContext.get_from_file ctx "a.ads"

let root = match AnalysisUnit.root u with Some n -> n | None -> assert false

let aux node =
  match node with
  | #BaseTypeDecl.t as type_decl -> (
    try
      let typ =
        Translation.translate_type_decl Translation.global_ctx type_decl
      in
      match BaseTypeDecl.f_name type_decl with
      | Some name
        when AdaNode.text name = "Unconstrained_Array_Ptr_Constrained"
             || AdaNode.text name = "My_Rec_Variant_Ptr_Constrained" -> (
        (* Special cases where we want to see what the access looks like *)
        match typ.desc with
        | Access (Object root_type) ->
            Format.printf "@[<v 2>%s@ @[access %a@]@]@.@."
              (AdaNode.short_image node)
              (IR.Typ.pp_full Translation.global_tenv)
              root_type
        | _ ->
            Format.printf "@[<v 2>Error translating %s:@ not an access@]@.@."
              (AdaNode.short_image type_decl) )
      | _ ->
          Format.printf "@[<v 2>%s@ @[%a@]@]@.@." (AdaNode.short_image node)
            (IR.Typ.pp_full Translation.global_tenv)
            typ
    with exn ->
      Format.printf "@[<v 2>Error translating %s:@ %s@]@.@."
        (AdaNode.short_image type_decl)
        (Exn.to_string exn) )
  | _ ->
      ()


let () = AdaNode.iter aux root
