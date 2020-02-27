open Libadalang

module Name = struct
  type t = [DefiningName.t | SubtypeIndication.t | AnonymousTypeDecl.t]

  let equal (a : t) (b : t) = AdaNode.equal a b

  let compare (a : t) (b : t) = AdaNode.compare a b

  let hash (a : t) = AdaNode.hash a
end

module Hashtbl = Caml.Hashtbl.Make (Name)
module Set = Caml.Set.Make (Name)
include Name

let from_defining_name (name : DefiningName.t) = (name :> t)

let from_subtype_indication (subtype : SubtypeIndication.t) = (subtype :> t)

let from_anonymous_type_decl (anon : AnonymousTypeDecl.t) = (anon :> t)

let pp fmt name = Format.pp_print_string fmt (AdaNode.text name)
