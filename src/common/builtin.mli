open! Base
open AbsLang

(** Reserved builtins. *)
type t =
  | Push
  | Insert
  | Remove
  | Spawn
  | Yield

(** Get builtin from identifier. *)
val of_ident : ident -> t option
