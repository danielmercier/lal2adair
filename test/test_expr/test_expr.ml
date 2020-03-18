open Libadalang

let ctx = AnalysisContext.create ()

let u = AnalysisContext.get_from_file ctx "a.adb"

let root = match AnalysisUnit.root u with Some n -> n | None -> assert false

let pp_assign fmt assign =
  let expr = AssignStmt.f_expr assign in
  try
    let lal_expr = Translation.translate_expr expr in
    Format.fprintf fmt "%s = %a" (AdaNode.short_image expr) IR.Expr.pp lal_expr
  with exn ->
    Format.fprintf fmt "@[<v 2>Error translating %s:@ %s@]"
      (AdaNode.short_image expr) (Exn.to_string exn)


let pp_subp fmt subp =
  let subp_spec = SubpBody.f_subp_spec subp in
  let name =
    AdaNode.text (Option.value_exn (SubpSpec.f_subp_name subp_spec))
  in
  let assigns = AdaNode.findall AssignStmt subp in
  Format.fprintf fmt "@[<v 2>%s:@ @[<v>%a@]@]" name
    (Format.pp_print_list pp_assign)
    assigns


let () =
  AdaNode.findall SubpBody root
  |> Format.printf "@[<v>%a@]@." (Format.pp_print_list pp_subp)
