open! Base
module Borrow_graph = Common.Borrow_graph
module Value = Common.Value
module Env = Value.Env

(** Move state for owned values. *)
type ownership =
  | Live
  | Moved

(** Active borrow target. *)
type borrow = Borrow_graph.target

(** Borrowchecker state. *)
type env =
  { owns : ownership Env.t
  ; graph : Borrow_graph.t
  }

(** Check moves and use-after-move errors. *)
val stmts : Core.stmt list -> env -> (unit * env, string) Result.t
