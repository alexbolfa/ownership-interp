open! Base

(** Shared or mutable borrow. *)
type ref_kind =
  | Shared
  | Mutable
[@@deriving compare, equal, sexp_of]

(** Runtime address referenced by a borrow. *)
type address =
  { root : string
  ; indices : int list
  }
[@@deriving compare, equal, sexp_of]

(** Runtime values. *)
type t =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of t list
  | VRef of ref_kind * address
[@@deriving compare, equal, sexp_of]

(** Value string representation. *)
val to_string : t -> string

(** Source program variable name. *)
val name : AbsLang.ident -> string

(** Key based storage *)
module Env : sig
  type 'a t = 'a Map.M(String).t

  (** Empty environment. *)
  val empty : 'a t

  (** Find a binding. *)
  val find : AbsLang.ident -> 'a t -> 'a option

  (** Add or replace a binding. *)
  val bind : AbsLang.ident -> 'a -> 'a t -> 'a t

  (** Fold over bindings. *)
  val fold : 'a t -> init:'b -> f:('b -> string -> 'a -> 'b) -> 'b
end

(** Function signature. *)
type tfunc = TFun of AbsLang.typeT list * AbsLang.typeT

(** Whether values of this source type transfer ownership when used. *)
val type_is_owned : AbsLang.typeT -> bool

(** Whether this source type contains any reference. *)
val type_has_ref : AbsLang.typeT -> bool

(** Whether this source type is a reference value. *)
val type_is_ref : AbsLang.typeT -> bool

(** Parameter name. *)
val param_ident : AbsLang.param -> AbsLang.ident

(** Parameter type. *)
val param_type : AbsLang.param -> AbsLang.typeT

(** Get type string representation. *)
val type_to_string : AbsLang.typeT -> string
