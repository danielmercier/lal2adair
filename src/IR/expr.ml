type t = {node: expr_node; orig_node: Libadalang.Expr.t; typ: Typ.t}

and expr_node =
  | Name of name
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
  [`DiscreteType of Typ.t * range_constraint option | `Range of range]

and range = DoubleDot of t * t | Range of type_or_name * int option

and type_expr = Typ.t * type_constraint option

and type_constraint = RangeConstraint of range_constraint

and range_constraint = t * t

and membership_kind = In | NotIn

and membership_choice = [`Expr of t | `Range of range | `Type of Typ.t]

and varinfo = Source of {vname: Name.t} | Undefined

and fieldinfo = {fieldname: Name.t}

and function_name = Cfun of funinfo | Pfun of name

and funinfo = {fname: Name.t}

and attribute_ref =
  | Access of fun_or_name
  | Unchecked_Access of fun_or_name
  | Unrestricted_Access of fun_or_name
  | Address of fun_or_name
  | First of type_or_name * int option
  | Last of type_or_name * int option
  | Length of type_or_name * int option
  | Result of funinfo

and type_or_expr = [`Type of Typ.t | `Expr of t]

and type_or_name = [`Type of Typ.t | `Name of name]

and fun_or_name = [`Fun of funinfo | `Name of name]

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
  let pp_type_expr fmt type_expr =
    match type_expr with
    | typ, Some (RangeConstraint (left, right)) ->
        Format.fprintf fmt "@[%a range %a .. %a@]" Typ.pp typ pp left pp right
    | typ, None ->
        Format.fprintf fmt "@[%a@]" Typ.pp typ
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
        Format.fprintf fmt "@[%a [%a]@]" pp_name name pp_discrete_range range
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
  and pp_fun_or_name fmt = function
    | `Name name ->
        Format.fprintf fmt "@[%a@]" pp_name name
    | `Fun {fname} ->
        Format.fprintf fmt "@[Fun(%a)@]" Name.pp fname
  and pp_type_or_name fmt = function
    | `Name name ->
        Format.fprintf fmt "@[%a@]" pp_name name
    | `Type typ ->
        Format.fprintf fmt "@[Type(%a)@]" Typ.pp typ
  and pp_attribute_ref fmt = function
    | Access prefix ->
        Format.fprintf fmt "@[%a'Access@]" pp_fun_or_name prefix
    | Unchecked_Access prefix ->
        Format.fprintf fmt "@[%a'Unchecked_Access@]" pp_fun_or_name prefix
    | Unrestricted_Access prefix ->
        Format.fprintf fmt "@[%a'Unrestricted_Access@]" pp_fun_or_name prefix
    | Address prefix ->
        Format.fprintf fmt "@[%a'Address@]" pp_fun_or_name prefix
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
          Format.fprintf fmt "of%a %a" pp_reversed reversed pp_name name
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
