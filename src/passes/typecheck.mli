open! Base
open AbsLang
module Value = Common.Value
module Env = Value.Env

(** Binding mutability. *)
type mutability =
  | Imm
  | Mut

(** Typechecker state. *)
type env =
  { vars : typeT Env.t
  ; muts : mutability Env.t
  ; funs : Value.tfunc Env.t
  ; ret : typeT option
  }

(** Typecheck core statements and return the same core shape with type annotations filled in. *)
val stmts : Core.stmt list -> env -> (Core.stmt list * env, string) Result.t
