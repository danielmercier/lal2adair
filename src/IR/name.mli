open Libadalang

type t

module Hashtbl : Caml.Hashtbl.S with type key := t

module Set : Caml.Set.S with type elt := t

val from_defining_name : DefiningName.t -> t

val from_subtype_indication : SubtypeIndication.t -> t
(** Some names are defined by a subtype indication *)

val from_anonymous_type_decl : AnonymousTypeDecl.t -> t
(** An anonymous type decl does not have defining name *)

val pp : Format.formatter -> t -> unit
