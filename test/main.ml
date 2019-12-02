open Libadalang

let ctx = AnalysisContext.create ()

let u = AnalysisContext.get_from_file ctx "a.adb"

let root = match AnalysisUnit.root u with Some n -> n | None -> assert false

let aux assign =
  let expr = AssignStmt.f_expr assign in
  try
    let lal_expr = Translation.Translate_expr.translate_expr expr in
    Format.printf "@[<v>%s = %a@ @]" (AdaNode.short_image expr) Ada_ir.Expr.pp
      lal_expr
  with _ ->
    Format.printf "@[<v>Error translating %s@ @]" (AdaNode.short_image expr)

let () = AdaNode.findall AssignStmt root |> List.iter ~f:aux
