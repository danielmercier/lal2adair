open Libadalang

exception Lal_error of string

exception Legality_error of string

let lal_error fmt = Format.kasprintf (fun msg -> raise (Lal_error msg)) fmt

let legality_error fmt =
  Format.kasprintf (fun msg -> raise (Legality_error msg)) fmt

let compute_name defining_name =
  { Ada_ir.Name.plain= AdaNode.text defining_name
  ; mangled= AdaNode.short_image defining_name }
