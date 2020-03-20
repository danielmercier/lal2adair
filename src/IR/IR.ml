module Int_lit = struct
  include Int_lit
end

module Name = struct
  include Name
end

module Enum = struct
  include Enum
end

module Mode = struct
  include Mode
end

module rec Typ : sig
  type aspect = Volatile

  type orig_node = [Libadalang.BaseTypeDecl.t | Libadalang.TypeExpr.t]

  (* TODO: tenv, array_name and record_name should both be opaque types.
     We cannot do it until we remove the 'include Typ' *)
  type array_name = Name.t

  type record_name = Name.t

  type t =
    { desc: desc
    ; aspects: aspect list
    ; name: type_name
    ; parent_type: t option
    ; orig_node: orig_node }

  and type_name =
    | StdCharacter
    | StdWideCharacter
    | StdWideWideCharacter
    | Address
    (* Identifies Boolean types (Ada RM 3.5.3) *)
    | Boolean
    | UniversalInteger
    | Name of Name.t

  and tenv =
    { typ_tbl: t Name.Hashtbl.t
    ; elt_typ: t Name.Hashtbl.t
    ; record_typ: record Name.Hashtbl.t }

  and desc =
    (* Discrete and Real are Scalar types (Ada RM 3.5) *)
    | Discrete of discrete_type
    | Real of real_type
    | Access of access_type
    (* Array types (Ada RM 3.6) *)
    | Array of array_name * index_constraint
    | Record of record_name * discriminant_constraint

  and discrete_type = discrete_type_desc * range_constraint option

  and discrete_type_desc =
    | Enum of enum_typ
    (* Integer types (Ada RM 3.5.4) *)
    | Signed of Int_lit.t * Int_lit.t
    | Modular of Int_lit.t

  (** Enumeration types (Ada RM 3.5.1) *)
  and enum_typ =
    | Character of Int_lit.t (* Length of the character type *)
    | OtherEnum of Enum.name list

  and subprogram_typ =
    | Function of {args: param list; ret_typ: t}
    | Procedure of {args: param list}

  and param = {pname: Name.t; ptyp: t; pmode: Mode.t}

  (** Real types (Ada RM 3.5.6) *)
  and real_type =
    (* TODO: Floating point types (Ada RM 3.5.7) *)
    | Float
    (* TODO: Fixed point types (Ada RM 3.5.9) *)
    | Fixed

  and range_constraint = Expr.range

  and index_constrained = IndexConstrained | IndexUnconstrained

  (** Index constraint (Ada RM 3.6.1)
      The indices are either all constrained or all unconstrained *)
  and index_constraint = index_constrained * discrete_type list

  and discr_type = [`Access of access_type | `Discrete of discrete_type]

  (* Discriminant (Ada RM 3.7) *)
  and discriminant = {discr_name: Name.t; discr_typ: discr_type}

  (** Discriminants constraints (Ada RM 3.7.1)
      Either all discriminants are constrained or unconstrained *)
  and discriminant_constraint =
    | DiscrConstrained of (Name.t * Expr.t) list
    | DiscrUnconstrained

  (** Record types (Ada RM 3.8) *)
  and record = {discriminants: discriminant list; fields: field list}

  (** Component (Ada RM 3.8 (6/3)) *)
  and field =
    {field_name: Name.t; field_typ: t; field_constraint: field_constraint}

  (** Variant part (Ada RM 3.8.1)
      All variant that lead to a field are translated to one constraint *)
  and alternatives = {discriminant: Name.t; choices: Expr.discrete_choice list}

  and field_constraint =
    {not_alternatives: alternatives list; alternatives: alternatives list}

  (** Access types (Ada RM 3.10)
      To avoid any recursion, if the root type is an object, use it's name
      instead *)
  and access_type =
    (* access to object (Ada RM 3.10 (3)) *)
    | Object of t
    | Anonymous of Name.t
    (* access to subprogram (Ada RM 3.10 (5)) *)
    | Subprogram of subprogram_typ

  val mk_empty_tenv : unit -> tenv

  val find_opt : tenv -> Name.t -> t option

  val elt_type : tenv -> array_name -> t

  val record_type : tenv -> record_name -> record

  val add : tenv -> Name.t -> t -> unit

  val add_elt_typ : tenv -> array_name -> t -> unit

  val add_record_typ : tenv -> record_name -> record -> unit

  (* Pretty print a full description of the type *)
  val pp_full : tenv -> Format.formatter -> t -> unit

  (* Pretty print the name of the type *)
  val pp : Format.formatter -> t -> unit

  val pp_discrete_type : Format.formatter -> discrete_type -> unit
end = struct
  include Typ

  let mk_empty_tenv () =
    { typ_tbl= Name.Hashtbl.create 256
    ; elt_typ= Name.Hashtbl.create 256
    ; record_typ= Name.Hashtbl.create 256 }


  let find_opt tenv name = Name.Hashtbl.find_opt tenv.typ_tbl name

  let elt_type tenv array_name = Name.Hashtbl.find tenv.elt_typ array_name

  let record_type tenv record_name =
    Name.Hashtbl.find tenv.record_typ record_name


  let add tenv name = Name.Hashtbl.add tenv.typ_tbl name

  let add_elt_typ tenv array_name = Name.Hashtbl.add tenv.elt_typ array_name

  let add_record_typ tenv record_name =
    Name.Hashtbl.add tenv.record_typ record_name


  let pp_enum_typ fmt = function
    | Character c ->
        Format.fprintf fmt "Character(%a)" Int_lit.pp c
    | OtherEnum enums ->
        let pp_sep fmt () = Format.fprintf fmt ",@ " in
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list ~pp_sep Enum.pp_name)
          enums


  let pp_discrete_type fmt (typ, range) =
    let pp_typ fmt typ =
      match typ with
      | Enum enum_typ ->
          pp_enum_typ fmt enum_typ
      | Signed (i1, i2) ->
          Format.fprintf fmt "signed (%a, %a)" Int_lit.pp i1 Int_lit.pp i2
      | Modular m ->
          Format.fprintf fmt "mod %a" Int_lit.pp m
    in
    match range with
    | Some range ->
        Format.fprintf fmt "@[<2>%a@ range %a@]" pp_typ typ Expr.pp_range range
    | None ->
        Format.fprintf fmt "%a" pp_typ typ


  let pp_real_type fmt = function
    | Float ->
        Format.pp_print_string fmt "Float"
    | Fixed ->
        Format.pp_print_string fmt "Fixed"


  let pp_access_type fmt = function
    | Object root_type ->
        Format.fprintf fmt "access %a" pp root_type
    | Anonymous name ->
        Format.fprintf fmt "access %a" Name.pp name
    | Subprogram _ ->
        Format.fprintf fmt "access subprogram"


  let rec pp_full tenv fmt typ =
    let pp_desc fmt desc =
      (* In that case, write the description of the type *)
      match desc with
      | Discrete discrete_typ ->
          pp_discrete_type fmt discrete_typ
      | Real real_typ ->
          pp_real_type fmt real_typ
      | Access access_type ->
          pp_access_type fmt access_type
      | Array (array_name, index_constraint) ->
          let elt_typ = elt_type tenv array_name in
          pp_array_type fmt (elt_typ, index_constraint)
      | Record (record_name, discriminant_constraint) ->
          let record = record_type tenv record_name in
          pp_record_type fmt (record, discriminant_constraint)
    in
    Format.fprintf fmt "@[<2>type %a is@ %a;@]" pp typ pp_desc typ.desc


  and pp_array_type fmt (elt_typ, index_constraint) =
    let pp_index_constraint fmt (index_constrained, indices) =
      let pp_index_constrained fmt discrete_type =
        match index_constrained with
        | IndexConstrained ->
            pp_discrete_type fmt discrete_type
        | IndexUnconstrained ->
            Format.fprintf fmt "%a range <>" pp_discrete_type discrete_type
      in
      let pp_sep fmt () = Format.fprintf fmt ",@ " in
      Format.fprintf fmt "%a"
        (Format.pp_print_list ~pp_sep pp_index_constrained)
        indices
    in
    Format.fprintf fmt "@[array (@[%a@])@ of %a@]" pp_index_constraint
      index_constraint pp elt_typ


  and pp_record_type fmt (record, discriminant_constraint) =
    let pp_field fmt field =
      let pp_field_constraint fmt field_constraint =
        let pp_alternatives fmt alternatives =
          let pp_sep fmt () = Format.fprintf fmt "@ | " in
          Format.fprintf fmt "%a in %a" Name.pp alternatives.discriminant
            (Format.pp_print_list ~pp_sep Expr.pp_discrete_choice)
            alternatives.choices
        in
        let pp_sep fmt () = Format.fprintf fmt "@ and " in
        let pp_alternatives_list =
          Format.pp_print_list ~pp_sep pp_alternatives
        in
        match
          (field_constraint.not_alternatives, field_constraint.alternatives)
        with
        | [], [] ->
            (* No constraint *)
            ()
        | [], alternatives ->
            Format.fprintf fmt "%a =>@ " pp_alternatives_list alternatives
        | not_alternatives, [] ->
            Format.fprintf fmt "not (%a) =>@ " pp_alternatives_list
              not_alternatives
        | not_alternatives, alternatives ->
            Format.fprintf fmt "not (%a) and %a =>@ " pp_alternatives_list
              not_alternatives pp_alternatives_list alternatives
      in
      Format.fprintf fmt "@[<2>%a@]%a : %a" pp_field_constraint
        field.field_constraint Name.pp field.field_name pp field.field_typ
    in
    let pp_discriminant_constraint fmt discriminant_constraint =
      let pp_disciminant fmt (discriminant, expr) =
        let pp_expr fmt = function
          | Some expr ->
              Format.fprintf fmt " => %a" Expr.pp expr
          | None ->
              ()
        in
        let pp_discr_typ fmt = function
          | `Access access_typ ->
              pp_access_type fmt access_typ
          | `Discrete discrete_type ->
              pp_discrete_type fmt discrete_type
        in
        Format.fprintf fmt "%a : %a%a" Name.pp discriminant.discr_name
          pp_discr_typ discriminant.discr_typ pp_expr expr
      in
      let discriminant_list =
        match discriminant_constraint with
        | DiscrConstrained constraints -> (
            let result =
              List.map2
                ~f:(fun discriminant (_, expr) -> (discriminant, Some expr))
                record.discriminants constraints
            in
            match result with Ok ok -> ok | Unequal_lengths -> assert false )
        | DiscrUnconstrained ->
            List.map
              ~f:(fun discriminant -> (discriminant, None))
              record.discriminants
      in
      match discriminant_list with
      | [] ->
          ()
      | _ ->
          let pp_sep fmt () = Format.fprintf fmt ",@ " in
          Format.fprintf fmt " (%a)"
            (Format.pp_print_list ~pp_sep pp_disciminant)
            discriminant_list
    in
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Format.fprintf fmt "@[<hv 2>record@[%a@]@ %a@ end record@]"
      pp_discriminant_constraint discriminant_constraint
      (Format.pp_print_list ~pp_sep pp_field)
      record.fields


  let pp fmt typ =
    match typ.name with
    | StdCharacter ->
        Format.pp_print_string fmt "Character"
    | StdWideCharacter ->
        Format.pp_print_string fmt "Wide_Character"
    | StdWideWideCharacter ->
        Format.pp_print_string fmt "Wide_Wide_Character"
    | Address ->
        Format.pp_print_string fmt "Address"
    | Boolean ->
        Format.pp_print_string fmt "Boolean"
    | UniversalInteger ->
        Format.pp_print_string fmt "Universal_Integer"
    | Name name ->
        Name.pp fmt name
end

and Expr : sig
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
    | Allocator of Typ.t * t option
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

  and discrete_range = [`DiscreteType of Typ.discrete_type | `Range of range]

  (* range is defined in Ada RM 3.5 *)
  and range = DoubleDot of t * t | Range of type_or_name * int option

  and membership_kind = In | NotIn

  and membership_choice = [`Expr of t | `Range of range | `Type of Typ.t]

  and varinfo = Source of {vname: Name.t; vdecl: vardecl} | Undefined

  and vardecl = {decl_scope: decl_scope; decl_kind: decl_kind}

  and decl_scope = FunScope of funinfo | PackageScope

  and decl_kind =
    | FormalVar of funinfo * Mode.t
    | ReturnVar
    | ForLoopVar of iterator_specification
    | Variable

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

  (** discrete_choice defined in Ada RM 3.8.1 *)
  and discrete_choice =
    [`Expr of t | `DiscreteType of Typ.discrete_type | `Range of range]

  and case_expr_alternative = {choices: discrete_choice list; when_expr: t}

  val pp : Format.formatter -> t -> unit

  val pp_range : Format.formatter -> range -> unit

  val pp_const : Format.formatter -> const -> unit

  val pp_discrete_choice : Format.formatter -> discrete_choice -> unit
end = struct
  include Expr

  let pp_const fmt = function
    | Int i ->
        Format.fprintf fmt "%a" Int_lit.pp i
    | String s ->
        Format.fprintf fmt "%s" s
    | Null ->
        Format.fprintf fmt "null"
    | Enum e ->
        Format.fprintf fmt "%a" Enum.pp e


  let rec pp fmt {node} =
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
      Format.fprintf fmt "(@[<hv 2>for %a %a =>@ %a@])" pp_quantifier
        quantifier pp_iterator_specification iterator_specification pp
        predicate
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
        Format.fprintf fmt "@[%a@]"
          (pp_array_aggregate pp_positional)
          aggregate
    | NamedArrayAggregate aggregate ->
        Format.fprintf fmt "@[%a@]" (pp_array_aggregate pp_named) aggregate
    | Allocator (typ, expr) ->
        let pp_expr fmt = function
          | Some e ->
              Format.fprintf fmt "'(%a)" pp e
          | None ->
              ()
        in
        Format.fprintf fmt "@[%a%a@]" Typ.pp typ pp_expr expr
    | If (condition, then_e, else_e) ->
        Format.fprintf fmt "@[if %a then %a%a@]" pp condition pp then_e pp
          else_e
    | Case (case_expr, case_alternatives, others_expr) ->
        Format.fprintf fmt "@[%a@]" pp_case_expr
          (case_expr, case_alternatives, others_expr)
    | Quantified (quantifier, iterator_specification, predicate) ->
        Format.fprintf fmt "@[%a@]" pp_quantified
          (quantifier, iterator_specification, predicate)


  and pp_offset : type a.
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
    | `DiscreteType discrete_type ->
        Typ.pp_discrete_type fmt discrete_type
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


  and pp_discrete_choice fmt = function
    | `Expr e ->
        pp fmt e
    | #discrete_range as range ->
        pp_discrete_range fmt range
end
