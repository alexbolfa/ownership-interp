open! Base
open AbsLang
module C = Core
module Value = Common.Value
module Env = Value.Env

(* Checks expression types, function signatures, arity, and mutability.
   This pass assumes names were already resolved by Lowering. *)

type mutability =
  | Imm
  | Mut

type env =
  { vars : typeT Env.t
  ; muts : mutability Env.t
  ; funs : Value.tfunc Env.t
  ; ret : typeT option
  }
[@@deriving accessors]

type checked_place =
  { place : C.place
  ; can_write : bool
  }

module AppState = struct
  type t = env
end

module Except = Monadic.Result.Make (struct
    type t = string
  end)

module AppM = Monadic.State.MakeT (Except) (AppState)

module M = struct
  module T = struct
    type 'a t = 'a AppM.t

    let return = AppM.pure
    let bind m ~f = AppM.bind m f
    let map = `Define_using_bind
  end

  include T
  include Base.Monad.Make (T)

  let lift = AppM.elevate
  let get = AppM.get
  let put = AppM.put
  let access accessor = AppM.create (fun s -> Except.pure (Accessor.get accessor s, s))

  let map_access accessor ~f : unit t =
    AppM.create (fun s -> Except.pure ((), Accessor.map accessor s ~f))
  ;;

  let scoped ~enter ~leave m =
    let open Let_syntax in
    let%bind outer = get in
    let%bind () = put (enter outer) in
    let%bind v = m in
    let%bind inner = get in
    let%map () = put (leave ~outer ~inner) in
    v
  ;;

  let fail msg = lift (Except.error msg)
  let run m env = Except.run (AppM.run m env)
end

open M.Let_syntax

let unreachable msg = failwith (Printf.sprintf "internal error (typechecker bug): %s" msg)
let typed typ node = { C.typ = C.Type typ; node }

let type_of (e : C.exp) =
  match e.C.typ with
  | C.Type t -> t
  | C.Any -> unreachable "expected typed core expression"
;;

let type_of_place place =
  match place.C.typ with
  | C.Type t -> t
  | C.Any -> unreachable "expected typed core place"
;;

let reject_type = function
  | (TRef t | TMutRef t) when Value.type_has_ref t ->
    M.fail "References cannot point to references."
  | TList elem when Value.type_has_ref elem ->
    M.fail "Lists of references require explicit lifetimes."
  | _ -> M.return ()
;;

let type_var x =
  let%bind var_env = M.access vars in
  match Env.find x var_env with
  | Some t -> M.return t
  | None ->
    let (Ident name) = x in
    unreachable (Printf.sprintf "variable %s missing after lower" name)
;;

let mutability x =
  match%bind M.access muts >>| Env.find x with
  | Some m -> M.return m
  | None -> unreachable "bound variable missing mutability"
;;

let bind_var x t m =
  let%bind () = reject_type t in
  let%bind () = M.map_access vars ~f:(Env.bind x t) in
  M.map_access muts ~f:(Env.bind x m)
;;

let rec check_place raw =
  let%bind root_typ = type_var raw.C.root in
  let%bind root_mut =
    match%map mutability raw.C.root with
    | Mut -> true
    | Imm -> false
  in
  let rec walk can_write blocked_by_shared current_typ typed_steps = function
    | [] ->
      M.return
        { place =
            { raw with
              C.typ = C.Type current_typ
            ; root_typ = C.Type root_typ
            ; steps = List.rev typed_steps
            }
        ; can_write
        }
    | C.PIndex index :: rest ->
      (match current_typ with
       | TList elem ->
         let%bind index = check index TInt in
         walk can_write blocked_by_shared elem (C.PIndex index :: typed_steps) rest
       | t ->
         M.fail (Printf.sprintf "Expected a list, but got %s" (Value.type_to_string t)))
    | C.PDeref :: rest ->
      (match current_typ with
       | TRef t -> walk false true t (C.PDeref :: typed_steps) rest
       | TMutRef t ->
         walk (not blocked_by_shared) blocked_by_shared t (C.PDeref :: typed_steps) rest
       | t ->
         M.fail
           (Printf.sprintf "Expected a reference, but got %s" (Value.type_to_string t)))
  in
  walk root_mut false root_typ [] raw.C.steps

and block body =
  M.scoped
    ~enter:Fn.id
    ~leave:(fun ~outer ~inner:_ -> outer)
    (M.all (List.map body ~f:stmt))

and stmt = function
  | C.SLet (x, e) ->
    let%bind e = expr e in
    let%map () = bind_var x (type_of e) Imm in
    C.SLet (x, e)
  | C.SLetMut (x, e) ->
    let%bind e = expr e in
    let%map () = bind_var x (type_of e) Mut in
    C.SLetMut (x, e)
  | C.SFun (f, params, tret, body) ->
    let%bind () =
      if Value.type_has_ref tret
      then M.fail "Functions cannot return references without explicit lifetimes."
      else M.return ()
    in
    let%bind outer = M.get in
    let funs =
      Env.bind f (Value.TFun (List.map params ~f:Value.param_type, tret)) outer.funs
    in
    let fn_env =
      List.fold params ~init:{ outer with funs; ret = Some tret } ~f:(fun env p ->
        let x = Value.param_ident p in
        let t = Value.param_type p in
        let m =
          match p with
          | ParamMut _ -> Mut
          | ParamImm _ -> Imm
        in
        { env with vars = Env.bind x t env.vars; muts = Env.bind x m env.muts })
    in
    let%bind () =
      M.all_unit (List.map params ~f:(fun p -> reject_type (Value.param_type p)))
    in
    let%bind () = M.put fn_env in
    let%bind body = M.all (List.map body ~f:stmt) in
    let%bind () = M.put { outer with funs } in
    let%bind () =
      let rec stmt_returns = function
        | C.SReturn _ -> true
        | C.SScope body -> block_returns body
        | C.SIfElse (_, then_body, else_body) ->
          block_returns then_body && block_returns else_body
        | _ -> false
      and block_returns stmts =
        match List.last stmts with
        | Some stmt -> stmt_returns stmt
        | None -> false
      in
      if block_returns body
      then M.return ()
      else M.fail (Printf.sprintf "Function %s is missing a return." (Value.name f))
    in
    M.return (C.SFun (f, params, tret, body))
  | C.SAssign (lhs, rhs) ->
    let%bind lhs = check_place lhs in
    let%bind () =
      if lhs.can_write
      then M.return ()
      else if
        List.exists lhs.place.steps ~f:(function
          | C.PDeref -> true
          | C.PIndex _ -> false)
      then M.fail "Cannot mutate through immutable reference."
      else
        M.fail
          (Printf.sprintf
             "Cannot mutate immutable variable %s."
             (Value.name lhs.place.root))
    in
    let%bind () =
      if List.is_empty lhs.place.steps && Value.type_is_ref (type_of_place lhs.place)
      then M.fail "Reference variables cannot be reassigned."
      else M.return ()
    in
    let%map rhs = check rhs (type_of_place lhs.place) in
    C.SAssign (lhs.place, rhs)
  | C.SScope body ->
    let%map body = block body in
    C.SScope body
  | C.SIfElse (cond, t_body, f_body) ->
    let%bind cond = check cond TBool in
    let%bind t_body = block t_body in
    let%map f_body = block f_body in
    C.SIfElse (cond, t_body, f_body)
  | C.SWhile (cond, body) ->
    let%bind cond = check cond TBool in
    let%map body = block body in
    C.SWhile (cond, body)
  | C.SReturn e ->
    (match%bind M.access ret with
     | Some expected ->
       let%map e = check e expected in
       C.SReturn e
     | None ->
       let%map e = expr e in
       C.SReturn e)
  | C.SExpr e ->
    let%map e = expr e in
    C.SExpr e

and check e expected =
  match e.C.node, expected with
  | C.EList es, TList elem ->
    let%bind () = reject_type expected in
    let%map es = M.all (List.map es ~f:(fun e -> check e elem)) in
    typed expected (C.EList es)
  | _ ->
    let compatible actual =
      Poly.(expected = actual)
      ||
      match expected, actual with
      | TRef a, TMutRef b -> Poly.(a = b)
      | _ -> false
    in
    let%bind actual = expr e in
    if compatible (type_of actual)
    then M.return { actual with C.typ = C.Type expected }
    else
      M.fail
        (Printf.sprintf
           "Expression should be of type %s, but it has type %s instead."
           (Value.type_to_string expected)
           (Value.type_to_string (type_of actual)))

and expr e =
  let bin ctor a b targ tret =
    let%bind a = check a targ in
    let%map b = check b targ in
    typed tret (ctor a b)
  in
  let call f args =
    let%bind (Value.TFun (targs, tret)) =
      let%bind fun_env = M.access funs in
      match Env.find f fun_env with
      | Some tfn -> M.return tfn
      | None ->
        let (Ident name) = f in
        unreachable (Printf.sprintf "function %s missing after lower" name)
    in
    let n_expected = List.length targs in
    let n_given = List.length args in
    if n_expected <> n_given
    then
      M.fail
        (Printf.sprintf
           "Function %s expects %d argument(s), but was given %d."
           (Value.name f)
           n_expected
           n_given)
    else (
      let%map args =
        M.all (List.map (List.zip_exn args targs) ~f:(fun (arg, t) -> check arg t))
      in
      args, tret)
  in
  match e.C.node with
  | C.EInt i -> M.return (typed TInt (C.EInt i))
  | C.EMul (a, b) -> bin (fun a b -> C.EMul (a, b)) a b TInt TInt
  | C.EDiv (a, b) -> bin (fun a b -> C.EDiv (a, b)) a b TInt TInt
  | C.EAdd (a, b) -> bin (fun a b -> C.EAdd (a, b)) a b TInt TInt
  | C.ETrue -> M.return (typed TBool C.ETrue)
  | C.EFalse -> M.return (typed TBool C.EFalse)
  | C.EAnd (a, b) -> bin (fun a b -> C.EAnd (a, b)) a b TBool TBool
  | C.EEq (a, b) ->
    let%bind a = expr a in
    let%map b = check b (type_of a) in
    typed TBool (C.EEq (a, b))
  | C.ELt (a, b) -> bin (fun a b -> C.ELt (a, b)) a b TInt TBool
  | C.EIf (c, t, f) ->
    let%bind c = check c TBool in
    let%bind t = expr t in
    let expected = type_of t in
    let%map f =
      match expected with
      | TRef _ | TMutRef _ ->
        let%bind actual = expr f in
        if Poly.(expected = type_of actual)
        then M.return actual
        else
          M.fail
            (Printf.sprintf
               "Expression should be of type %s, but it has type %s instead."
               (Value.type_to_string expected)
               (Value.type_to_string (type_of actual)))
      | _ -> check f expected
    in
    typed (type_of t) (C.EIf (c, t, f))
  | C.EVar x ->
    let%map t = type_var x in
    typed t (C.EVar x)
  | C.EBorrow (kind, place) ->
    let%bind place = check_place place in
    let%bind () =
      if
        List.exists place.place.steps ~f:(function
          | C.PDeref -> true
          | C.PIndex _ -> false)
      then M.fail "Cannot borrow through a reference."
      else M.return ()
    in
    let%bind () =
      if Value.type_has_ref (type_of_place place.place)
      then M.fail "References cannot point to references."
      else M.return ()
    in
    let%bind () =
      match kind with
      | Value.Shared -> M.return ()
      | Value.Mutable ->
        if place.can_write
        then M.return ()
        else if
          List.exists place.place.steps ~f:(function
            | C.PDeref -> true
            | C.PIndex _ -> false)
        then M.fail "Cannot borrow through immutable reference as mutable."
        else
          M.fail
            (Printf.sprintf
               "Cannot borrow immutable variable %s as mutable."
               (Value.name place.place.root))
    in
    let typ =
      match kind with
      | Value.Shared -> TRef (type_of_place place.place)
      | Value.Mutable -> TMutRef (type_of_place place.place)
    in
    M.return (typed typ (C.EBorrow (kind, place.place)))
  | C.EDeref e ->
    let%bind e = expr e in
    (match type_of e with
     | TRef t | TMutRef t -> M.return (typed t (C.EDeref e))
     | t ->
       M.fail (Printf.sprintf "Expected a reference, but got %s" (Value.type_to_string t)))
  | C.EList [] -> M.fail "Cannot infer element type of empty list literal"
  | C.EList (head :: rest) ->
    let%bind head = expr head in
    let elem = type_of head in
    let%bind () = reject_type (TList elem) in
    let%map rest = M.all (List.map rest ~f:(fun e -> check e elem)) in
    typed (TList elem) (C.EList (head :: rest))
  | C.EIndex (base, idx) ->
    (match%bind expr base with
     | { C.typ = C.Type (TList elem); _ } as base ->
       let%map idx = check idx TInt in
       typed elem (C.EIndex (base, idx))
     | base ->
       M.fail
         (Printf.sprintf
            "Expected a list, but got %s"
            (Value.type_to_string (type_of base))))
  | C.EListMutate (xs, op) ->
    let%bind xs_type =
      let%bind t = type_var xs in
      match%bind mutability xs with
      | Mut -> M.return t
      | Imm ->
        M.fail (Printf.sprintf "Cannot mutate immutable variable %s." (Value.name xs))
    in
    (match xs_type with
     | TList elem ->
       let type_op =
         match op with
         | C.Push x ->
           let%map x = check x elem in
           C.Push x
         | C.Insert (i, x) ->
           let%bind i = check i TInt in
           let%map x = check x elem in
           C.Insert (i, x)
         | C.Remove i ->
           let%map i = check i TInt in
           C.Remove i
       in
       let%map op = type_op in
       typed TUnit (C.EListMutate (xs, op))
     | t -> M.fail (Printf.sprintf "Expected a list, but got %s" (Value.type_to_string t)))
  | C.ECall (f, args) ->
    let%map args, tret = call f args in
    typed tret (C.ECall (f, args))
  | C.ESpawn (f, args) ->
    let%bind args, _ = call f args in
    let%bind () =
      match List.find args ~f:(fun arg -> Value.type_has_ref (type_of arg)) with
      | Some _ -> M.fail "Spawned threads cannot take reference arguments."
      | None -> M.return ()
    in
    M.return (typed TUnit (C.ESpawn (f, args)))
  | C.EYield -> M.return (typed TUnit C.EYield)
;;

let stmts stmts env = M.run (M.all (List.map stmts ~f:stmt)) env
