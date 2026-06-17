open! Base
open AbsLang

type t =
  | Push
  | Insert
  | Remove
  | Spawn
  | Yield

let of_ident (Ident name) =
  match name with
  | "push" -> Some Push
  | "insert" -> Some Insert
  | "remove" -> Some Remove
  | "spawn" -> Some Spawn
  | "yield" -> Some Yield
  | _ -> None
;;
