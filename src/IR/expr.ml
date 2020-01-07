type 'a with_data = {node: 'a; orig_node: Libadalang.Expr.t; typ: Typ.t}

type t = expr_node with_data

and expr_node =
  | Name of name
  | AttributeRef of attribute_ref
  | Const of const
  | Membership of t * membership_kind * membership_choice list
  | Raise of Name.t * t option
  | Unop of unop * t
  | Binop of binop * t * t
  | RecordAggregate of record_aggregate
  | NullRecordAggregate
  | NamedArrayAggregate of named array_aggregate
  | PositionalArrayAggregate of t array_aggregate
  | Allocator of type_expr * t option
  | If of t * t * t
  | Case of t * case_expr_alternative list * t option
  | Quantified of quantifier * iterator_specification * t

and quantifier = ForAll | Exists

and iterator_specification =
  {var_name: Name.t; reversed: bool; iter_kind: iter_kind}

and iter_kind = Iterator of discrete_range | Iterable of name

and name = name_node with_data

and name_node =
  | Lval of lval
  | Cast of cast with_offset
  | QualExpr of qual_expr with_offset
  | FunctionCall of function_call with_offset
  | AccessOf of access_kind * fun_or_lval

and cast = Typ.t * t

and qual_expr = Typ.t * t

and function_call = function_name * param list

and param =
  | InParam of t
  | OutParam of lval_or_cast with_data
  | InOutParam of lval_or_cast with_data

and lval_or_cast = [`Lval of lval | `Cast of Typ.t * lval_or_cast]

and access_kind = Access | Unchecked_Access | Unrestricted_Access | Address

and lval = base * offset

and base = Var of varinfo | Mem of name

and offset = offset_node with_data

and offset_node =
  | NoOffset
  | Index of offset * t list
  | Slice of offset * discrete_range
  | Field of offset * fieldinfo

and 'a with_offset = 'a * offset

and const = Int of Int_lit.t | String of string | Null | Enum of Enum.t

and discrete_range =
  [`DiscreteType of Typ.t * range_constraint option | `Range of range]

and range = DoubleDot of t * t | Range of type_or_name * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and membership_kind = In | NotIn

and membership_choice = [`Expr of t | `Range of range | `Type of Typ.t]

and varinfo = Source of {vname: Name.t; vdecl: vardecl} | Undefined

and vardecl = {decl_scope: decl_scope; decl_kind: decl_kind}

and decl_scope = FunScope of funinfo | PackageScope

and decl_kind =
  | FormalVar of funinfo * varmode
  | ReturnVar
  | ForLoopVar of iterator_specification
  | Variable

and varmode = ModeIn | ModeOut | ModeInOut

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | First of type_or_name * int option
  | Last of type_or_name * int option
  | Length of type_or_name * int option
  | Result of funinfo

and type_or_expr = [`Type of Typ.t | `Expr of t]

and type_or_name = [`Type of Typ.t | `Name of name]

and fun_or_lval = [`Fun of funinfo | `Lval of lval]

and unop = Abs | Not | UnaryMinus | UnaryPlus

and binop =
  | And
  | Or
  | OrElse
  | AndThen
  | Xor
  | Pow
  | Mult
  | Div
  | Mod
  | Rem
  | Plus
  | Minus
  | Concat
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte

and record_aggregate = record_association list

and record_association = {field: fieldinfo; expr: aggregate_expr}

and aggregate_expr = Expr of t | Default

and 'a array_aggregate = {assoc: 'a list; others: t option}

and named = {index: discrete_choice list; aggregate_expr: t}

and discrete_choice =
  [ `Expr of t
  | `DiscreteType of Typ.t * range_constraint option
  | `Range of range ]

and case_expr_alternative = {choices: discrete_choice list; when_expr: t}

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
  let pp_type_expr fmt type_expr =
    match type_expr with
    | typ, Some (RangeConstraint (left, right)) ->
        Format.fprintf fmt "@[%a range %a .. %a@]" Typ.pp typ pp left pp right
    | typ, None ->
        Format.fprintf fmt "@[%a@]" Typ.pp typ
  in
  let rec pp_offset : type a.
      (Format.formatter -> a -> unit) -> Format.formatter -> a * offset -> unit
      =
   fun pp_base fmt (base, offset) ->
    match offset.node with
    | NoOffset ->
        pp_base fmt base
    | Index (offset, indicies) ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "%a [%a]" (pp_offset pp_base) (base, offset)
          (Format.pp_print_list ~pp_sep pp)
          indicies
    | Slice (offset, range) ->
        Format.fprintf fmt "@[%a [%a]@]" (pp_offset pp_base) (base, offset)
          pp_discrete_range range
    | Field (offset, {fieldname}) ->
        Format.fprintf fmt "%a.%a" (pp_offset pp_base) (base, offset) Name.pp
          fieldname
  and pp_base fmt = function
    | Var Undefined ->
        Format.pp_print_string fmt "undefined"
    | Var (Source {vname}) ->
        Name.pp fmt vname
    | Mem name ->
        Format.fprintf fmt "%a.all" pp_name name.node
  and pp_lval fmt lval = pp_offset pp_base fmt lval
  and pp_cast fmt (typ, e) = Format.fprintf fmt "@[%a (%a)@]" Typ.pp typ pp e
  and pp_qual_expr fmt (typ, e) =
    Format.fprintf fmt "@[%a'(%a)@]" Typ.pp typ pp e
  and pp_name fmt = function
    | Lval lval ->
        pp_lval fmt lval
    | Cast cast ->
        pp_offset pp_cast fmt cast
    | QualExpr qual_expr ->
        pp_offset pp_qual_expr fmt qual_expr
    | FunctionCall function_call ->
        pp_offset pp_function_call fmt function_call
    | AccessOf (access_kind, fun_or_lval) ->
        Format.fprintf fmt "@[%a'%a@]" pp_access_kind access_kind
          pp_fun_or_lval fun_or_lval
  and pp_access_kind fmt = function
    | Access ->
        Format.pp_print_string fmt "Access"
    | Unchecked_Access ->
        Format.pp_print_string fmt "Unchecked_Access"
    | Unrestricted_Access ->
        Format.pp_print_string fmt "Unrestricted_Access"
    | Address ->
        Format.pp_print_string fmt "Address"
  and pp_function_call fmt function_call =
    let rec pp_lval_or_cast fmt = function
      | `Lval lval ->
          pp_lval fmt lval
      | `Cast (typ, lval_or_cast) ->
          Format.fprintf fmt "%a (%a)" Typ.pp typ pp_lval_or_cast lval_or_cast
    in
    let pp_arg fmt = function
      | InParam e ->
          pp fmt e
      | OutParam lval_or_cast | InOutParam lval_or_cast ->
          pp_lval_or_cast fmt lval_or_cast.node
    in
    let pp_args =
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.pp_print_list ~pp_sep pp_arg
    in
    match function_call with
    | Cfun {fname}, args ->
        Format.fprintf fmt "@[<hv 2>%a@ (@[%a@])@]" Name.pp fname pp_args args
    | Pfun name, args ->
        Format.fprintf fmt "@[<hv 2>%a.all@ (@[%a@])@]" pp_name name.node
          pp_args args
  and pp_index_arg fmt = function
    | Some index ->
        Format.fprintf fmt "(%d)" index
    | None ->
        ()
  and pp_range fmt = function
    | DoubleDot (left, right) ->
        Format.fprintf fmt "@[%a .. %a@]" pp left pp right
    | Range (range_prefix, index) ->
        Format.fprintf fmt "@[%a'Range%a@]" pp_type_or_name range_prefix
          pp_index_arg index
  and pp_discrete_range fmt = function
    | `DiscreteType (typ, Some (left, right)) ->
        Format.fprintf fmt "@[%a range %a .. %a@]" Typ.pp typ pp left pp right
    | `DiscreteType (typ, None) ->
        Format.fprintf fmt "@[%a no range@]" Typ.pp typ
    | `Range range ->
        pp_range fmt range
  and pp_fun_or_lval fmt = function
    | `Lval lval ->
        pp_lval fmt lval
    | `Fun {fname} ->
        Format.fprintf fmt "@[Fun(%a)@]" Name.pp fname
  and pp_type_or_name fmt = function
    | `Name name ->
        Format.fprintf fmt "@[%a@]" pp_name name.node
    | `Type typ ->
        Format.fprintf fmt "@[Type(%a)@]" Typ.pp typ
  and pp_attribute_ref fmt = function
    | First (prefix, index) ->
        Format.fprintf fmt "@[%a'First%a@]" pp_type_or_name prefix pp_index_arg
          index
    | Last (prefix, index) ->
        Format.fprintf fmt "@[%a'Last%a@]" pp_type_or_name prefix pp_index_arg
          index
    | Length (prefix, index) ->
        Format.fprintf fmt "@[%a'Length%a@]" pp_type_or_name prefix
          pp_index_arg index
    | Result {fname} ->
        Format.fprintf fmt "@[%a'Result@]" Name.pp fname
  in
  let pp_membership_choices fmt choices =
    let pp_choice fmt = function
      | `Expr e ->
          Format.fprintf fmt "@[Expr(%a)@]" pp e
      | `Type typ ->
          Format.fprintf fmt "@[Type(%a)@]" Typ.pp typ
      | `Range range ->
          Format.fprintf fmt "@[Range(%a)@]" pp_range range
    in
    let pp_sep fmt () = Format.fprintf fmt "@ | " in
    Format.fprintf fmt "@[%a@]"
      (Format.pp_print_list ~pp_sep pp_choice)
      choices
  in
  let pp_unop fmt op =
    let str =
      match op with
      | Abs ->
          "abs "
      | Not ->
          "not "
      | UnaryMinus ->
          "-"
      | UnaryPlus ->
          "+"
    in
    Format.pp_print_string fmt str
  in
  let pp_binop fmt op =
    let str =
      match op with
      | And ->
          "and"
      | Or ->
          "or"
      | OrElse ->
          "or else"
      | AndThen ->
          "and then"
      | Xor ->
          "xor"
      | Pow ->
          "**"
      | Mult ->
          "*"
      | Div ->
          "\\"
      | Mod ->
          "mod"
      | Rem ->
          "rem"
      | Plus ->
          "+"
      | Minus ->
          "-"
      | Concat ->
          "&"
      | Eq ->
          "="
      | Neq ->
          "/="
      | Lt ->
          "<"
      | Lte ->
          "<="
      | Gt ->
          ">"
      | Gte ->
          ">="
    in
    Format.pp_print_string fmt str
  in
  let pp_record_aggregate fmt record_aggregate =
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    let pp_record_association fmt {field; expr} =
      match expr with
      | Expr e ->
          Format.fprintf fmt "@[%a => %a@]" Name.pp field.fieldname pp e
      | Default ->
          Format.fprintf fmt "@[%a => <>@]" Name.pp field.fieldname
    in
    Format.fprintf fmt "(@[%a@])"
      (Format.pp_print_list ~pp_sep pp_record_association)
      record_aggregate
  in
  let pp_discrete_choice fmt = function
    | `Expr e ->
        pp fmt e
    | #discrete_range as range ->
        pp_discrete_range fmt range
  in
  let pp_positional fmt assoc = pp fmt assoc in
  let pp_named fmt assoc =
    let pp_sep fmt () = Format.fprintf fmt " |@ " in
    Format.fprintf fmt "@[%a => %a@]"
      (Format.pp_print_list ~pp_sep pp_discrete_choice)
      assoc.index pp assoc.aggregate_expr
  in
  let pp_array_aggregate pp_assoc fmt aggregate =
    let pp_others fmt = function
      | Some e ->
          Format.fprintf fmt ",@ others => %a" pp e
      | None ->
          ()
    in
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.fprintf fmt "(@[%a%a@])"
      (Format.pp_print_list ~pp_sep pp_assoc)
      aggregate.assoc pp_others aggregate.others
  in
  let pp_case_expr fmt (case_expr, case_alternatives, others_expr) =
    let pp_others fmt = function
      | Some e ->
          Format.fprintf fmt ",@ when others => %a" pp e
      | None ->
          ()
    in
    let pp_case_expr_alternative fmt alternative =
      let pp_sep fmt () = Format.fprintf fmt "@ | " in
      Format.fprintf fmt "when @[%a => @[%a@]@]"
        (Format.pp_print_list ~pp_sep pp_discrete_choice)
        alternative.choices pp alternative.when_expr
    in
    let pp_alternatives =
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.pp_print_list ~pp_sep pp_case_expr_alternative
    in
    Format.fprintf fmt "@[<hv 2>case %a is@ %a%a@]" pp case_expr
      pp_alternatives case_alternatives pp_others others_expr
  in
  let pp_iterator_specification fmt {var_name; reversed; iter_kind} =
    let pp_reversed fmt reversed =
      if reversed then Format.pp_print_string fmt " reversed"
    in
    let pp_iter_kind fmt = function
      | Iterator range ->
          Format.fprintf fmt "in%a %a" pp_reversed reversed pp_discrete_range
            range
      | Iterable name ->
          Format.fprintf fmt "of%a %a" pp_reversed reversed pp_name name.node
    in
    Format.fprintf fmt "%a %a" Name.pp var_name pp_iter_kind iter_kind
  in
  let pp_quantified fmt (quantifier, iterator_specification, predicate) =
    let pp_quantifier fmt = function
      | ForAll ->
          Format.pp_print_string fmt "all"
      | Exists ->
          Format.pp_print_string fmt "some"
    in
    Format.fprintf fmt "(@[<hv 2>for %a %a =>@ %a@])" pp_quantifier quantifier
      pp_iterator_specification iterator_specification pp predicate
  in
  match node with
  | Name name ->
      Format.fprintf fmt "@[%a@]" pp_name name.node
  | Const const ->
      Format.fprintf fmt "@[%a@]" pp_const const
  | AttributeRef attribute_ref ->
      Format.fprintf fmt "@[%a@]" pp_attribute_ref attribute_ref
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
  | Unop (op, expr) ->
      Format.fprintf fmt "@[%a%a@]" pp_unop op pp expr
  | Binop (op, lexpr, rexpr) ->
      Format.fprintf fmt "@[%a %a %a@]" pp lexpr pp_binop op pp rexpr
  | RecordAggregate record_aggregate ->
      Format.fprintf fmt "@[%a@]" pp_record_aggregate record_aggregate
  | NullRecordAggregate ->
      Format.fprintf fmt "@[(null record)@]"
  | PositionalArrayAggregate aggregate ->
      Format.fprintf fmt "@[%a@]" (pp_array_aggregate pp_positional) aggregate
  | NamedArrayAggregate aggregate ->
      Format.fprintf fmt "@[%a@]" (pp_array_aggregate pp_named) aggregate
  | Allocator (type_expr, expr) ->
      let pp_expr fmt = function
        | Some e ->
            Format.fprintf fmt "'(%a)" pp e
        | None ->
            ()
      in
      Format.fprintf fmt "@[%a%a@]" pp_type_expr type_expr pp_expr expr
  | If (condition, then_e, else_e) ->
      Format.fprintf fmt "@[if %a then %a%a@]" pp condition pp then_e pp else_e
  | Case (case_expr, case_alternatives, others_expr) ->
      Format.fprintf fmt "@[%a@]" pp_case_expr
        (case_expr, case_alternatives, others_expr)
  | Quantified (quantifier, iterator_specification, predicate) ->
      Format.fprintf fmt "@[%a@]" pp_quantified
        (quantifier, iterator_specification, predicate)
