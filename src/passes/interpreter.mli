open! Base
open Async
module Value = Common.Value
module Env = Value.Env

(** Runtime function body. *)
type func = Fun of AbsLang.ident list * Core.stmt list

(** Runtime state. *)
type env =
  { vars : Value.t Env.t
  ; funs : func Env.t
  ; threads : unit Deferred.t list
  ; error : string Ivar.t
  }

(** Execute checked statements. *)
val stmts : Core.stmt list -> env -> (Value.t option * env, string) Result.t Deferred.t

(** Evaluate a checked expression. *)
val expression : Core.exp -> env -> (Value.t * env, string) Result.t Deferred.t
