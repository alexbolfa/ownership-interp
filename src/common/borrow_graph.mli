open! Base

(** Persistent graph of active borrow relationships.

    A stored edge [owner -> root] means the reference variable [owner] keeps
    [root] borrowed. A temporary edge has no owner and lasts only until the end
    of the current statement. *)

type target =
  { root : AbsLang.ident
  ; kind : Value.ref_kind
  }

type t

val empty : t
val add_temporary : target -> t -> t
val add_owned : owner:AbsLang.ident -> target -> t -> t
val add_owned_all : owner:AbsLang.ident -> target list -> t -> t
val remove_owner : AbsLang.ident -> t -> t
val owned_by : AbsLang.ident -> t -> target list
val active : ?ignore:AbsLang.ident list -> AbsLang.ident -> t -> target list
val merge : t -> t -> t
val merge_stored : t -> t -> t
val keep_outer : outer:t -> inner:t -> t
val clear_temporaries : t -> t
val set_temporaries : t -> target list -> t
val temporaries : t -> target list
