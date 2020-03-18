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
      Format.printf "@[<v 2>%s@ @[%a@]@]@.@." (AdaNode.short_image node)
        IR.Typ.pp_full typ
    with exn ->
      Format.printf "@[<v 2>Error translating %s:@ %s@]@.@."
        (AdaNode.short_image type_decl)
        (Exn.to_string exn) )
  | _ ->
      ()


let () = AdaNode.iter aux root
