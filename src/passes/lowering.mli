open! Base

(** Lowering/name-resolution state. *)
type env =
  { vars : AbsLang.ident Map.M(String).t
  ; funs : AbsLang.ident Map.M(String).t
  ; next : int
  }

val stmts : AbsLang.stmt list -> env -> (Core.stmt list * env, string) Result.t
