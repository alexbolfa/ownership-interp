open! Base

(** Persistent list used by runtime list. *)
type 'a t = 'a list

(** Get by zero-based index. *)
val get : 'a t -> int -> ('a, string) Result.t

(** Replace by zero-based index. *)
val set : 'a t -> int -> 'a -> ('a t, string) Result.t

(** Append an item. *)
val push : 'a t -> 'a -> 'a t

(** Insert before a zero-based index. *)
val insert : 'a t -> int -> 'a -> ('a t, string) Result.t

(** Remove by zero-based index. *)
val remove : 'a t -> int -> ('a t, string) Result.t
