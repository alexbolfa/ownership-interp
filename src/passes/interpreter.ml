open! Base
open Async
module C = Core
module Plist = Common.Plist
module Value = Common.Value
module Env = Value.Env
open C

(* Evaluates a lowered, typechecked, and borrowchecked program.
   Runtime lookups and type/value shape checks are internal errors except for
   dynamic failures such as division by zero and list bounds errors. *)

type func = Fun of AbsLang.ident list * C.stmt list

and env =
  { vars : Value.t Env.t
  ; funs : func Env.t
  ; threads : unit Deferred.t list
  ; error : string Ivar.t
  }
[@@deriving accessors]

module AppState = struct
  type t = env
end

module Deferred_base = struct
  type 'a t = 'a Deferred.t

  let pure = Deferred.return
  let map f m = Deferred.map m ~f

  let apply mf mx =
    let open Deferred.Let_syntax in
    let%bind f = mf in
    let%map x = mx in
    f x
  ;;

  let bind m f = Deferred.bind m ~f
  let join m = Deferred.bind m ~f:Fn.id
end

module Except =
  Monadic.Result.MakeT
    (Deferred_base)
    (struct
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
  let lift_result result = lift (Except.create (Deferred.return result))
  let lift_deferred m = lift (Except.elevate m)
  let run m env = Except.run (AppM.run m env)
end

open M.Let_syntax

let yield = M.lift_deferred (Scheduler.yield ())
let unreachable msg = failwith ("internal error (interpreter bug): " ^ msg)
let key (AbsLang.Ident name) = name
let ident key = AbsLang.Ident key

let var x =
  let%bind var_env = M.access vars in
  match Env.find x var_env with
  | Some v -> M.return v
  | None ->
    let (AbsLang.Ident name) = x in
    unreachable (Printf.sprintf "variable %s is not bound" name)
;;

let rec list_expr = function
  | Value.VList xs -> M.return xs
  | _ -> unreachable "expected list"

and ref_expr = function
  | Value.VRef (_, addr) -> M.return addr
  | _ -> unreachable "expected reference"

and read_path v = function
  | [] -> M.return v
  | i :: rest ->
    let%bind xs = list_expr v in
    let%bind v = M.lift_result (Plist.get xs i) in
    read_path v rest

and read_addr ({ root; indices } : Value.address) =
  let%bind root = var (ident root) in
  read_path root indices

and addr_of_place place =
  let rec walk addr = function
    | [] -> M.return addr
    | PIndex index :: rest ->
      let%bind index = int_expr index in
      let%bind container = read_addr addr in
      let%bind values = list_expr container in
      let%bind _ = M.lift_result (Plist.get values index) in
      walk { addr with Value.indices = addr.Value.indices @ [ index ] } rest
    | PDeref :: rest ->
      let%bind v = read_addr addr in
      let%bind addr = ref_expr v in
      walk addr rest
  in
  walk { Value.root = key place.root; indices = [] } place.steps

and int_expr e =
  let%bind v = expr e in
  match v with
  | Value.VInt i -> M.return i
  | _ -> unreachable "expected int"

and bool_expr e =
  let%bind v = expr e in
  match v with
  | Value.VBool b -> M.return b
  | _ -> unreachable "expected bool"

and expr (e : C.exp) : Value.t M.t =
  let arith a b op =
    let%bind a = int_expr a in
    let%map b = int_expr b in
    Value.VInt (op a b)
  in
  match e.node with
  | EInt i -> M.return (Value.VInt i)
  | ETrue -> M.return (Value.VBool true)
  | EFalse -> M.return (Value.VBool false)
  | EMul (a, b) -> arith a b ( * )
  | EDiv (a, b) ->
    let%bind a = int_expr a in
    let%bind b = int_expr b in
    if b = 0 then M.fail "division by zero" else M.return (Value.VInt (a / b))
  | EAdd (a, b) -> arith a b ( + )
  | EAnd (a, b) ->
    let%bind a = bool_expr a in
    if a then expr b else M.return (Value.VBool false)
  | EEq (a, b) ->
    let%bind a = expr a in
    let%map b = expr b in
    Value.VBool (Value.equal a b)
  | ELt (a, b) ->
    let%bind a = int_expr a in
    let%map b = int_expr b in
    Value.VBool (a < b)
  | EIf (cond, t, f) ->
    let%bind cond = bool_expr cond in
    if cond then expr t else expr f
  | EVar x -> var x
  | EBorrow (kind, place) ->
    let%map addr = addr_of_place place in
    Value.VRef (kind, addr)
  | EDeref e ->
    let%bind v = expr e in
    let%bind addr = ref_expr v in
    read_addr addr
  | EList es ->
    let%map vs = M.all (List.map es ~f:expr) in
    Value.VList vs
  | EIndex (base, index) ->
    let%bind base = expr base in
    let%bind xs = list_expr base in
    let%bind index = int_expr index in
    M.lift_result (Plist.get xs index)
  | EListMutate (xs, op) ->
    let update values =
      let%map () = M.map_access vars ~f:(Env.bind xs (Value.VList values)) in
      Value.VUnit
    in
    let%bind v = var xs in
    let%bind values = list_expr v in
    (match op with
     | C.Push item ->
       let%bind item = expr item in
       update (Plist.push values item)
     | C.Insert (index, item) ->
       let%bind index = int_expr index in
       let%bind item = expr item in
       let%bind values = M.lift_result (Plist.insert values index item) in
       update values
     | C.Remove index ->
       let%bind index = int_expr index in
       let%bind values = M.lift_result (Plist.remove values index) in
       update values)
  | ECall (f, args) -> call f args
  | ESpawn (f, args) ->
    let%bind () = spawn f args in
    M.return Value.VUnit
  | EYield ->
    let%map () = yield in
    Value.VUnit

and call f args =
  let%bind (Fun (params, body)) =
    let%bind fun_env = M.access funs in
    match Env.find f fun_env with
    | Some fn -> M.return fn
    | None -> unreachable "call to undefined function"
  in
  let%bind args = M.all (List.map args ~f:expr) in
  M.scoped
    ~enter:(fun env ->
      let vars =
        List.fold2_exn params args ~init:env.vars ~f:(fun vars param value ->
          Env.bind param value vars)
      in
      { env with vars })
    ~leave:(fun ~outer ~inner ->
      { outer with vars = merge_outer_vars outer inner; threads = inner.threads })
    (let%bind result = stmt_list body in
     match result with
     | Some v -> M.return v
     | None -> unreachable "function body missing return")

and spawn f args =
  let call_values f args =
    let%bind (Fun (params, body)) =
      let%bind fun_env = M.access funs in
      match Env.find f fun_env with
      | Some fn -> M.return fn
      | None -> unreachable "call to undefined function"
    in
    M.scoped
      ~enter:(fun env ->
        let vars =
          List.fold2_exn params args ~init:env.vars ~f:(fun vars param value ->
            Env.bind param value vars)
        in
        { env with vars })
      ~leave:(fun ~outer ~inner ->
        { outer with vars = merge_outer_vars outer inner; threads = inner.threads })
      (let%bind result = stmt_list body in
       match result with
       | Some v -> M.return v
       | None -> unreachable "function body missing return")
  in
  let%bind args = M.all (List.map args ~f:expr) in
  let%bind env = M.get in
  let thread =
    let open Deferred.Let_syntax in
    let spawned_env = { env with threads = [] } in
    let%map result = M.run (call_values f args) spawned_env in
    match result with
    | Ok _ -> ()
    | Error msg -> if not (Ivar.is_full env.error) then Ivar.fill_exn env.error msg
  in
  M.map_access threads ~f:(fun threads -> thread :: threads)

and stmt : C.stmt -> Value.t option M.t = function
  | SLet (x, e) | SLetMut (x, e) ->
    let%bind v = expr e in
    let%map () = M.map_access vars ~f:(Env.bind x v) in
    None
  | SFun (f, params, _ret, body) ->
    let params = List.map params ~f:Value.param_ident in
    let%map () = M.map_access funs ~f:(Env.bind f (Fun (params, body))) in
    None
  | SAssign (place, rhs) ->
    let%bind addr = addr_of_place place in
    let%bind rhs = expr rhs in
    let rec write_path v path rhs =
      match path with
      | [] -> M.return rhs
      | i :: rest ->
        let%bind xs = list_expr v in
        let%bind old = M.lift_result (Plist.get xs i) in
        let%bind updated = write_path old rest rhs in
        M.lift_result (Result.map (Plist.set xs i updated) ~f:(fun xs -> Value.VList xs))
    in
    let write_addr ({ root; indices } : Value.address) rhs =
      let%bind root_value = var (ident root) in
      let%bind value = write_path root_value indices rhs in
      M.map_access vars ~f:(Env.bind (ident root) value)
    in
    let%map () = write_addr addr rhs in
    None
  | SScope body -> scoped_stmts body
  | SIfElse (cond, t_body, f_body) ->
    let%bind cond = bool_expr cond in
    if cond then scoped_stmts t_body else scoped_stmts f_body
  | SWhile (cond, body) ->
    let rec loop () =
      let%bind cond = bool_expr cond in
      if cond
      then (
        match%bind scoped_stmts body with
        | None -> loop ()
        | Some _ as result -> M.return result)
      else M.return None
    in
    loop ()
  | SReturn e ->
    let%map v = expr e in
    Some v
  | SExpr e ->
    let%map _ = expr e in
    None

and stmt_list : C.stmt list -> Value.t option M.t = function
  | [] -> M.return None
  | s :: rest ->
    (match%bind stmt s with
     | None -> stmt_list rest
     | Some _ as result -> M.return result)

and merge_outer_vars outer inner =
  Env.fold outer.vars ~init:Env.empty ~f:(fun vars name outer_value ->
    let id = AbsLang.Ident name in
    Env.bind id (Option.value (Env.find id inner.vars) ~default:outer_value) vars)

and scoped_stmts stmts =
  M.scoped
    ~enter:Fn.id
    ~leave:(fun ~outer ~inner ->
      { outer with vars = merge_outer_vars outer inner; threads = inner.threads })
    (stmt_list stmts)
;;

let fresh_runtime env = { env with threads = []; error = Ivar.create () }

let run_root m env =
  let open Deferred.Let_syntax in
  let%bind result = M.run m (fresh_runtime env) in
  match result with
  | Error msg -> Deferred.return (Error msg)
  | Ok (v, env) ->
    let wait_for_threads env =
      match env.threads with
      | [] -> Deferred.return (Ok ())
      | threads ->
        let all_done =
          Deferred.map (Deferred.all_unit (List.rev threads)) ~f:(fun () -> Ok ())
        in
        let first_error = Deferred.map (Ivar.read env.error) ~f:(fun msg -> Error msg) in
        Deferred.any [ all_done; first_error ]
    in
    let%map thread_result = wait_for_threads env in
    (match thread_result with
     | Error msg -> Error msg
     | Ok () -> Ok (v, fresh_runtime env))
;;

let stmts stmts env = run_root (stmt_list stmts) env
let expression e env = run_root (expr e) env
