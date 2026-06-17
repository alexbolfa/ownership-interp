open! Base
open AbsLang
module Value = Common.Value

(** Type annotation on core expressions and places.

    [Any] is used after lowering and before typechecking. [Type _] is filled
    in by the typechecker and consumed by later passes. *)
type typ =
  | Any
  | Type of typeT

(** Mutating list operation lowered from surface builtins. *)
type list_op =
  | Push of exp
  | Insert of exp * exp
  | Remove of exp

(** One navigation step inside a normalized assignment target. *)
and place_step =
  | PIndex of exp
  | PDeref

(** Core expression with a mutable-by-replacement type annotation slot. *)
and exp =
  { typ : typ
  ; node : exp_node
  }

(** Normalized assignment target. [root] is the variable being assigned through;
    [steps] is empty for direct variable assignment. *)
and place =
  { root : ident
  ; steps : place_step list
  ; typ : typ
  ; root_typ : typ
  }

and exp_node =
  | EInt of int
  | ETrue
  | EFalse
  | EMul of exp * exp
  | EDiv of exp * exp
  | EAdd of exp * exp
  | EAnd of exp * exp
  | EEq of exp * exp
  | ELt of exp * exp
  | EIf of exp * exp * exp
  | EVar of ident
  | EBorrow of Value.ref_kind * place
  | EDeref of exp
  | ECall of ident * exp list
  | ESpawn of ident * exp list
  | EYield
  | EList of exp list
  | EIndex of exp * exp
  | EListMutate of ident * list_op

(** Core statements. One-armed if-statements have been lowered away. *)
type stmt =
  | SLet of ident * exp
  | SLetMut of ident * exp
  | SFun of ident * param list * typeT * stmt list
  | SAssign of place * exp
  | SScope of stmt list
  | SIfElse of exp * stmt list * stmt list
  | SWhile of exp * stmt list
  | SReturn of exp
  | SExpr of exp

(** Build an untyped expression. *)
val exp : exp_node -> exp
