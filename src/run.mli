open! Base
open Async
module Borrowcheck = Passes.Borrowcheck
module Lowering = Passes.Lowering
module Interpreter = Passes.Interpreter
module Typecheck = Passes.Typecheck
module Value = Common.Value

(** Parse a complete program. *)
val parse : string -> (AbsLang.program, string) Result.t

(** Run a complete program. *)
val run : string -> (Value.t, string) Result.t

(** Run a complete program through Async. *)
val run_async : string -> (Value.t, string) Result.t Deferred.t

(** Program state. *)
type env =
  { names : Lowering.env
  ; tc : Typecheck.env
  ; bc : Borrowcheck.env
  ; interp : Interpreter.env
  ; line : int
  }

(** Empty program state. *)
val empty_env : env

(** Run set of statements. *)
val run_line : string -> env -> (Value.t option * env, string) Result.t

(** Run set of statements through Async. *)
val run_line_async : string -> env -> (Value.t option * env, string) Result.t Deferred.t

(** Check one line without evaluating it. *)
val check_line : string -> env -> (env, string) Result.t
