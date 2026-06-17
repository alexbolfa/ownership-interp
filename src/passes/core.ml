open! Base
open AbsLang
module Value = Common.Value

type typ =
  | Any
  | Type of typeT

type list_op =
  | Push of exp
  | Insert of exp * exp
  | Remove of exp

and place_step =
  | PIndex of exp
  | PDeref

and exp =
  { typ : typ
  ; node : exp_node
  }

and place =
  { root : ident
  ; steps : place_step list
  ; typ : typ
  ; root_typ : typ
  }

and exp_node =
  (* Primitives *)
  | EInt of int
  | ETrue
  | EFalse
  (* Int-like *)
  | EMul of exp * exp
  | EDiv of exp * exp
  | EAdd of exp * exp
  (* Boolean-like *)
  | EAnd of exp * exp
  | EEq of exp * exp
  | ELt of exp * exp
  | EIf of exp * exp * exp
  (* Vars *)
  | EVar of ident
  | EBorrow of Value.ref_kind * place
  | EDeref of exp
  | ECall of ident * exp list
  | ESpawn of ident * exp list
  | EYield
  (* Lists *)
  | EList of exp list
  | EIndex of exp * exp
  | EListMutate of ident * list_op

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

let exp node = { typ = Any; node }
