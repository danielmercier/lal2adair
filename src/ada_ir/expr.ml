type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Const of const
  | Lval of lval
  | CallExpr of called_expr * t list
  | AccessOf of access_kind * lval
  | Cast of Typ.t * t

and lval = lhost * offset

and lhost =
  | Var of varinfo
  | CustomVar of custom_var
  | CallHost of called_expr * t list
  | Mem of t

and offset =
  | Field of fieldinfo * offset
  | Index of t list * offset
  | NoOffset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

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
