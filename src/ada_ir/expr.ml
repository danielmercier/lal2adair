type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Const of const
  | Lval of lval
  | CallExpr of called_expr * t list
  | AccessOf of access_kind * lval
  | Membership of t * membership_kind * membership_choice list
  | Cast of Typ.t * t
  | QualExpr of Typ.t * t

and lval = lhost * offset

and lhost =
  | Var of varinfo
  | CustomVar of custom_var
  | CallHost of called_expr * t list
  | Mem of t

and offset =
  | Field of fieldinfo * offset
  | Index of t list * offset
  | Slice of discrete_range * offset
  | NoOffset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and discrete_range =
  | DiscreteType of Typ.t * range_constraint option
  | DiscreteRange of range

and range = DoubleDot of t * t | Range of range_prefix * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and range_prefix = Type of Typ.t | Array of lval

and membership_kind = In | NotIn

and membership_choice =
  | ChoiceExpr of t
  | ChoiceRange of range
  | ChoiceType of Typ.t

and varinfo = {vname: Name.t}

and custom_var = Undefined

and fieldinfo = {fieldname: Name.t}

and called_expr = Cfun of funinfo | Pfun of t

and funinfo = {fname: Name.t}

and access_kind = Access | Unchecked_Access | Unrestriced_Access | Address

let undefined () = Lval (CustomVar Undefined, NoOffset)

let rec pp fmt {node} =
  let pp_const fmt = function
    | Int i ->
        Format.fprintf fmt "%a" Int_lit.pp i
    | String s ->
        Format.fprintf fmt "%s" s
    | Null ->
        Format.fprintf fmt "null"
    | Enum e ->
        Format.fprintf fmt "%a" Enum.pp e
  in
  let pp_call_expr fmt call_expr =
    let pp_args =
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.pp_print_list ~pp_sep pp
    in
    match call_expr with
    | Cfun {fname}, args ->
        Format.fprintf fmt "@[<hv 2>%a@ (@[%a@])@]" Name.pp fname pp_args args
    | Pfun expr, args ->
        Format.fprintf fmt "@[<hv 2>%a.all@ (@[%a@])@]" pp expr pp_args args
  in
  let pp_lhost fmt = function
    | Var {vname} ->
        Name.pp fmt vname
    | CallHost (called_expr, args) ->
        pp_call_expr fmt (called_expr, args)
    | CustomVar Undefined ->
        Format.pp_print_string fmt "undefined"
    | Mem e ->
        Format.fprintf fmt "%a.all" pp e
  in
  let rec pp_lval fmt lval =
    match lval with
    | lhost, NoOffset ->
        pp_lhost fmt lhost
    | lhost, Field ({fieldname}, offset) ->
        Format.fprintf fmt "%a.%a" pp_lval (lhost, offset) Name.pp fieldname
    | lhost, Index (index, offset) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a [%a]" pp_lval (lhost, offset)
          (Format.pp_print_list ~pp_sep pp)
          index
    | lhost, Slice (range, offset) ->
        Format.fprintf fmt "@[%a [%a]@]" pp_lval (lhost, offset) pp_slice_range
          range
  and pp_range_prefix fmt = function
    | Type typ ->
        Format.fprintf fmt "Type(%a)" Typ.pp typ
    | Array arr ->
        Format.fprintf fmt "Array(%a)" pp_lval arr
  and pp_range fmt = function
    | DoubleDot (left, right) ->
        Format.fprintf fmt "@[%a .. %a@]" pp left pp right
    | Range (range_prefix, index_opt) ->
        let pp_index_opt fmt = function
          | Some i ->
              Format.fprintf fmt "(%d)" i
          | None ->
              Format.fprintf fmt ""
        in
        Format.fprintf fmt "@[%a'Range%a@]" pp_range_prefix range_prefix
          pp_index_opt index_opt
  and pp_slice_range fmt = function
    | DiscreteType (typ, Some (left, right)) ->
        Format.fprintf fmt "@[%a range %a .. %a@]" Typ.pp typ pp left pp right
    | DiscreteType (typ, None) ->
        Format.fprintf fmt "@[%a no range@]" Typ.pp typ
    | DiscreteRange range ->
        pp_range fmt range
  in
  let pp_access_kind fmt = function
    | Access ->
        Format.pp_print_string fmt "Access"
    | Unchecked_Access ->
        Format.pp_print_string fmt "Unchecked_Access"
    | Unrestriced_Access ->
        Format.pp_print_string fmt "Unrestriced_Access"
    | Address ->
        Format.pp_print_string fmt "Address"
  in
  let pp_membership_choices fmt choices =
    let pp_choice fmt = function
      | ChoiceExpr e ->
          Format.fprintf fmt "@[Expr(%a)@]" pp e
      | ChoiceType typ ->
          Format.fprintf fmt "@[Type(%a)@]" Typ.pp typ
      | ChoiceRange range ->
          Format.fprintf fmt "@[Range(%a)@]" pp_range range
    in
    let pp_sep fmt () = Format.fprintf fmt "@ | " in
    Format.fprintf fmt "@[%a@]"
      (Format.pp_print_list ~pp_sep pp_choice)
      choices
  in
  match node with
  | Const const ->
      Format.fprintf fmt "@[%a@]" pp_const const
  | Lval lval ->
      Format.fprintf fmt "@[%a@]" pp_lval lval
  | CallExpr (called_expr, args) ->
      Format.fprintf fmt "@[%a@]" pp_call_expr (called_expr, args)
  | AccessOf (access_kind, lval) ->
      Format.fprintf fmt "@[%a'%a@]" pp_lval lval pp_access_kind access_kind
  | Cast (typ, e) ->
      Format.fprintf fmt "@[%a (%a)@]" Typ.pp typ pp e
  | QualExpr (typ, e) ->
      Format.fprintf fmt "@[%a'(%a)@]" Typ.pp typ pp e
  | Membership (expr, kind, choices) ->
      let pp_kind fmt = function
        | In ->
            Format.pp_print_string fmt "in"
        | NotIn ->
            Format.pp_print_string fmt "not in"
      in
      Format.fprintf fmt "@[%a %a %a@]" pp expr pp_kind kind
        pp_membership_choices choices
