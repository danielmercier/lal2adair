type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Name of name
  | Const of const
  | Membership of t * membership_kind * membership_choice list
  | Raise of Name.t * t option

and name =
  | Var of varinfo
  | Enum of Enum.t
  | Deref of name
  | Index of name * t list
  | Slice of name * discrete_range
  | Field of name * fieldinfo
  | AttributeRef of attribute_ref
  | Cast of Typ.t * t
  | FunctionCall of function_name * t list
  | QualExpr of Typ.t * t

and const = Int of Int_lit.t | String of string | Null

and discrete_range =
  | DiscreteType of Typ.t * range_constraint option
  | DiscreteRange of range

and range = DoubleDot of t * t | Range of range_prefix * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and range_prefix = Type of Typ.t | Array of name

and membership_kind = In | NotIn

and membership_choice =
  | ChoiceExpr of t
  | ChoiceRange of range
  | ChoiceType of Typ.t

and varinfo = Source of {vname: Name.t} | Undefined

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | NameAccess of access_kind * name
  | FunAccess of access_kind * funinfo

and access_kind = Access | Unchecked_Access | Unrestriced_Access | Address

let undefined () = Var Undefined

let rec pp fmt {node} =
  let pp_const fmt = function
    | Int i ->
        Format.fprintf fmt "%a" Int_lit.pp i
    | String s ->
        Format.fprintf fmt "%s" s
    | Null ->
        Format.fprintf fmt "null"
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
  let rec pp_name fmt = function
    | Var (Source {vname}) ->
        Name.pp fmt vname
    | Var Undefined ->
        Format.pp_print_string fmt "undefined"
    | Enum e ->
        Format.fprintf fmt "%a" Enum.pp e
    | Deref name ->
        Format.fprintf fmt "%a.all" pp_name name
    | Index (name, indicies) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a [%a]" pp_name name
          (Format.pp_print_list ~pp_sep pp)
          indicies
    | Slice (name, range) ->
        Format.fprintf fmt "@[%a [%a]@]" pp_name name pp_slice_range range
    | Field (name, {fieldname}) ->
        Format.fprintf fmt "%a.%a" pp_name name Name.pp fieldname
    | AttributeRef attribute_ref ->
        Format.fprintf fmt "@[%a@]" pp_attribute_ref attribute_ref
    | Cast (typ, e) ->
        Format.fprintf fmt "@[%a (%a)@]" Typ.pp typ pp e
    | FunctionCall (function_name, args) ->
        Format.fprintf fmt "@[%a@]" pp_function_call (function_name, args)
    | QualExpr (typ, e) ->
        Format.fprintf fmt "@[%a'(%a)@]" Typ.pp typ pp e
  and pp_function_call fmt function_call =
    let pp_args =
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.pp_print_list ~pp_sep pp
    in
    match function_call with
    | Cfun {fname}, args ->
        Format.fprintf fmt "@[<hv 2>%a@ (@[%a@])@]" Name.pp fname pp_args args
    | Pfun name, args ->
        Format.fprintf fmt "@[<hv 2>%a.all@ (@[%a@])@]" pp_name name pp_args
          args
  and pp_range_prefix fmt = function
    | Type typ ->
        Format.fprintf fmt "Type(%a)" Typ.pp typ
    | Array arr ->
        Format.fprintf fmt "Array(%a)" pp_name arr
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
  and pp_attribute_ref fmt = function
    | NameAccess (kind, name) ->
        Format.fprintf fmt "@[%a'%a@]" pp_name name pp_access_kind kind
    | FunAccess (kind, {fname}) ->
        Format.fprintf fmt "@[%a'%a@]" Name.pp fname pp_access_kind kind
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
  | Name name ->
      Format.fprintf fmt "@[%a@]" pp_name name
  | Membership (expr, kind, choices) ->
      let pp_kind fmt = function
        | In ->
            Format.pp_print_string fmt "in"
        | NotIn ->
            Format.pp_print_string fmt "not in"
      in
      Format.fprintf fmt "@[%a %a %a@]" pp expr pp_kind kind
        pp_membership_choices choices
  | Raise (name, msg) ->
      let pp_msg fmt msg =
        match msg with
        | Some msg ->
            Format.fprintf fmt " with %a" pp msg
        | None ->
            ()
      in
      Format.fprintf fmt "@[raise %a%a@]" Name.pp name pp_msg msg
