open! Base

type ref_kind =
  | Shared
  | Mutable
[@@deriving compare, equal, sexp_of]

type address =
  { root : string
  ; indices : int list
  }
[@@deriving compare, equal, sexp_of]

type t =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of t list
  | VRef of ref_kind * address
[@@deriving compare, equal, sexp_of]

let name_string name =
  match String.rsplit2 name ~on:'#' with
  | Some (base, suffix) when String.for_all suffix ~f:Char.is_digit -> base
  | _ -> name
;;

let name (AbsLang.Ident name) = name_string name

let rec to_string v =
  match v with
  | VInt x -> Int.to_string x
  | VBool b -> Bool.to_string b
  | VUnit -> "()"
  | VList xs ->
    xs |> List.map ~f:to_string |> String.concat ~sep:", " |> Printf.sprintf "[%s]"
  | VRef (kind, { root; indices }) ->
    let kind =
      match kind with
      | Shared -> "&"
      | Mutable -> "&mut "
    in
    let indices = indices |> List.map ~f:(Printf.sprintf "[%d]") |> String.concat in
    kind ^ name_string root ^ indices
;;

module Env = struct
  type 'a t = 'a Map.M(String).t

  let key (AbsLang.Ident s) = s
  let empty : 'a t = Map.empty (module String)
  let bind id v env = Map.set env ~key:(key id) ~data:v
  let find id env = Map.find env (key id)
  let fold env ~init ~f = Map.fold env ~init ~f:(fun ~key ~data acc -> f acc key data)
end

type tfunc = TFun of AbsLang.typeT list * AbsLang.typeT

let rec type_has_ref = function
  | AbsLang.TRef _ | AbsLang.TMutRef _ -> true
  | AbsLang.TList t -> type_has_ref t
  | AbsLang.TInt | AbsLang.TBool | AbsLang.TUnit -> false
;;

let type_is_ref = function
  | AbsLang.TRef _ | AbsLang.TMutRef _ -> true
  | AbsLang.TInt | AbsLang.TBool | AbsLang.TUnit | AbsLang.TList _ -> false
;;

let type_is_owned = function
  | AbsLang.TList _ | AbsLang.TMutRef _ -> true
  | AbsLang.TInt | AbsLang.TBool | AbsLang.TUnit | AbsLang.TRef _ -> false
;;

let param_ident = function
  | AbsLang.ParamImm (id, _) | AbsLang.ParamMut (id, _) -> id
;;

let param_type = function
  | AbsLang.ParamImm (_, t) | AbsLang.ParamMut (_, t) -> t
;;

let type_to_string t = PrintLang.printTree PrintLang.prtTypeT t
