open Libadalang

exception Lal_error of string

exception Legality_error of string

let lal_error fmt = Format.kasprintf (fun msg -> raise (Lal_error msg)) fmt

let legality_error fmt =
  Format.kasprintf (fun msg -> raise (Legality_error msg)) fmt


let log_warning fmt = Format.printf fmt

let pp_node fmt n = Format.pp_print_string fmt (AdaNode.short_image n)

let defining_name name =
  match (name :> Name.t) with
  | #DefiningName.t as defining_name ->
      defining_name
  | _ -> (
    try
      match Name.p_referenced_defining_name name with
      | Some def_name ->
          def_name
      | None ->
          lal_error "Cannot find a defining name for %a" pp_node name
    with PropertyError s ->
      lal_error "Cannot find a defining name for %a: PropertyError %s" pp_node
        name s )


let referenced_subp_spec name =
  try
    match Name.p_referenced_decl name with
    | Some decl -> (
      match BasicDecl.p_subp_spec_or_null decl with
      | Some subp_spec ->
          subp_spec
      | None ->
          legality_error "%a should refer to a subprogram" pp_node decl )
    | None ->
        lal_error "Cannot find declaration for %a" pp_node name
  with PropertyError s ->
    lal_error "Cannot find declaration for %a: PropertyError %s" pp_node name s


let accessed_subp_spec name =
  try
    match Expr.p_expression_type name with
    | Some typ ->
        let rec access_subp_spec typ =
          let full_typ =
            match BaseTypeDecl.p_full_view typ with
            | Some full_typ ->
                full_typ
            | None ->
                typ
          in
          match full_typ with
          | #TypeDecl.t as type_decl -> (
              match%nolazy TypeDecl.f_type_def type_decl with
              | `AccessToSubpDef {f_subp_spec} ->
                  (f_subp_spec :> BaseSubpSpec.t)
              | #DerivedTypeDef.t -> (
                match BaseTypeDecl.p_base_type typ with
                | Some base_type ->
                    access_subp_spec base_type
                | None ->
                    lal_error "Cannot find base type for DerivedTypeDef %a"
                      pp_node typ )
              | _ ->
                  legality_error "%a should have an access to subprogram type"
                    pp_node name )
          | _ ->
              legality_error "%a should have an access to subprogram type"
                pp_node name
        in
        access_subp_spec typ
    | None ->
        lal_error "Cannot find type expression for %a" pp_node name
  with PropertyError s ->
    lal_error "Cannot find accessed subp spec for %a: PropertyError %s" pp_node
      name s


(** Type for various ada attributes *)
type attribute =
  [ `Access
  | `Unchecked_Access
  | `Unrestricted_Access
  | `Address
  | `Length
  | `First
  | `Last
  | `Range
  | `Val
  | `Pos
  | `Succ
  | `Pred
  | `Result
  | `Unknown of string ]

(* Association list for strings to the attribute.
 * we use this list to fill a hashtbl in get_attribute, to return the right
 * attribute according to the text of the attribute identifier *)
let attributes_assoc =
  [ ("access", `Access)
  ; ("unchecked_access", `Unchecked_Access)
  ; ("unrestricted_access", `Unrestricted_Access)
  ; ("address", `Address)
  ; ("first", `First)
  ; ("last", `Last)
  ; ("length", `Length)
  ; ("range", `Range)
  ; ("val", `Val)
  ; ("pos", `Pos)
  ; ("succ", `Succ)
  ; ("pred", `Pred)
  ; ("result", `Result) ]


let attribute =
  let tbl = Caml.Hashtbl.create (List.length attributes_assoc) in
  (* Fill the table with the association list above *)
  List.iter
    ~f:(fun (key, value) -> Caml.Hashtbl.add tbl key value)
    attributes_assoc ;
  fun attribute ->
    let str = String.lowercase (AdaNode.text attribute) in
    Option.value (Caml.Hashtbl.find_opt tbl str) ~default:(`Unknown str)
