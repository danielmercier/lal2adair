type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Const of const
  | Lval of lval
  | CallExpr of called_expr * t list
  | AccessOf of access_kind * lval

and lval = lhost * offset

and lhost = Var of varinfo

and offset = NoOffset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and varinfo = {vname: Name.t}

and called_expr = Cfun of funinfo | Pfun of t

and funinfo = {fname: Name.t}

and access_kind = Access | Unchecked_Access | Unrestriced_Access | Address

let undefined () =
  Lval (Var {vname= {plain= "undefined"; mangled= "_standard"}}, NoOffset)

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
  let pp_lval fmt lval =
    match lval with
    | Var {vname= {plain}}, NoOffset ->
        Format.pp_print_string fmt plain
  in
  let pp_call_expr fmt call_expr =
    let pp_args =
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.pp_print_list ~pp_sep pp
    in
    match call_expr with
    | Cfun {fname= {plain}}, args ->
        Format.fprintf fmt "@[<hv 2>%s@ (@[%a@])@]" plain pp_args args
    | Pfun expr, args ->
        Format.fprintf fmt "@[<hv 2>%a.all@ (@[%a@])@]" pp expr pp_args args
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
