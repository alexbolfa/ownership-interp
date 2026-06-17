open! Base
open AbsLang
module Builtin = Common.Builtin
module C = Core
module Value = Common.Value

type env =
  { vars : ident Map.M(String).t
  ; funs : ident Map.M(String).t
  ; next : int
  }
[@@deriving accessors]

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

  let fail msg = lift (Except.error msg)
  let run m env = Except.run (AppM.run m env)
end

open M.Let_syntax

let name (Ident name) = name

let fresh (Ident name) =
  let%bind next_id = M.access next in
  let id = Ident (Printf.sprintf "%s#%d" name next_id) in
  let%map () = M.map_access next ~f:(( + ) 1) in
  id
;;

let fresh_var x =
  let%bind x' = fresh x in
  let%map () = M.map_access vars ~f:(fun vars -> Map.set vars ~key:(name x) ~data:x') in
  x'
;;

let var x =
  match%bind M.access vars >>| fun vars -> Map.find vars (name x) with
  | Some x -> M.return x
  | None ->
    let (Ident name) = x in
    M.fail (Printf.sprintf "Variable %s is not bound" name)
;;

let rec exp e =
  let bin ctor a b =
    let%bind a = exp a in
    let%map b = exp b in
    C.exp (ctor a b)
  in
  let not_ e = C.exp (C.EIf (e, C.exp C.EFalse, C.exp C.ETrue)) in
  let neg e = C.exp (C.EMul (C.exp (C.EInt (-1)), e)) in
  match e with
  | EInt i -> M.return (C.exp (C.EInt i))
  | EMul (a, b) -> bin (fun a b -> C.EMul (a, b)) a b
  | EDiv (a, b) -> bin (fun a b -> C.EDiv (a, b)) a b
  | EAdd (a, b) -> bin (fun a b -> C.EAdd (a, b)) a b
  | ESub (a, b) ->
    let%bind a = exp a in
    let%map b = exp b in
    C.exp (C.EAdd (a, neg b))
  | ETrue -> M.return (C.exp C.ETrue)
  | EFalse -> M.return (C.exp C.EFalse)
  | ENot e ->
    let%map e = exp e in
    not_ e
  | EAnd (a, b) -> bin (fun a b -> C.EAnd (a, b)) a b
  | EOr (a, b) ->
    let%bind a = exp a in
    let%map b = exp b in
    C.exp (C.EIf (a, C.exp C.ETrue, b))
  | EEq (a, b) -> bin (fun a b -> C.EEq (a, b)) a b
  | ELt (a, b) -> bin (fun a b -> C.ELt (a, b)) a b
  | EGt (a, b) -> bin (fun a b -> C.ELt (b, a)) a b
  | ELeq (a, b) ->
    let%bind a = exp a in
    let%map b = exp b in
    not_ (C.exp (C.ELt (b, a)))
  | EGeq (a, b) ->
    let%bind a = exp a in
    let%map b = exp b in
    not_ (C.exp (C.ELt (a, b)))
  | EIf (c, t, f) ->
    let%bind c = exp c in
    let%bind t = exp t in
    let%map f = exp f in
    C.exp (C.EIf (c, t, f))
  | EVar x ->
    let%map x = var x in
    C.exp (C.EVar x)
  | EBorrow lhs ->
    let%map lhs = place [] lhs in
    C.exp (C.EBorrow (Value.Shared, lhs))
  | EBorrowMut lhs ->
    let%map lhs = place [] lhs in
    C.exp (C.EBorrow (Value.Mutable, lhs))
  | EDeref e ->
    let%map e = exp e in
    C.exp (C.EDeref e)
  | ECall (f, args) ->
    (match Builtin.of_ident f, args with
     | Some Builtin.Push, [ EVar xs; x ] ->
       let%bind xs = var xs in
       let%map x = exp x in
       C.exp (C.EListMutate (xs, C.Push x))
     | Some Builtin.Insert, [ EVar xs; i; x ] ->
       let%bind xs = var xs in
       let%bind i = exp i in
       let%map x = exp x in
       C.exp (C.EListMutate (xs, C.Insert (i, x)))
     | Some Builtin.Remove, [ EVar xs; i ] ->
       let%bind xs = var xs in
       let%map i = exp i in
       C.exp (C.EListMutate (xs, C.Remove i))
     | Some Builtin.Spawn, [ ECall (f, args) ] ->
       (match%bind M.access funs >>| fun funs -> Map.find funs (name f) with
        | Some f ->
          let%map args = M.all (List.map args ~f:exp) in
          C.exp (C.ESpawn (f, args))
        | None ->
          let (Ident name) = f in
          M.fail (Printf.sprintf "Function %s not defined" name))
     | Some Builtin.Yield, [] -> M.return (C.exp C.EYield)
     | Some builtin, _ ->
       let name, arity =
         match builtin with
         | Builtin.Push -> "push", 2
         | Builtin.Insert -> "insert", 3
         | Builtin.Remove -> "remove", 2
         | Builtin.Spawn -> "spawn", 1
         | Builtin.Yield -> "yield", 0
       in
       if Poly.(builtin = Builtin.Spawn)
       then M.fail "Builtin spawn expects a function call argument."
       else if Poly.(builtin = Builtin.Yield)
       then M.fail "Builtin yield expects no arguments."
       else
         M.fail
           (Printf.sprintf
              "Builtin %s expects a variable list argument and %d total argument(s)."
              name
              arity)
     | None, _ ->
       (match%bind M.access funs >>| fun funs -> Map.find funs (name f) with
        | Some f ->
          let%map args = M.all (List.map args ~f:exp) in
          C.exp (C.ECall (f, args))
        | None ->
          let (Ident name) = f in
          M.fail (Printf.sprintf "Function %s not defined" name)))
  | EList es ->
    let%map es = M.all (List.map es ~f:exp) in
    C.exp (C.EList es)
  | EIndex (base, index) ->
    let%bind base = exp base in
    let%map index = exp index in
    C.exp (C.EIndex (base, index))

and place steps = function
  | LVar x ->
    let%map root = var x in
    { C.root; steps; typ = C.Any; root_typ = C.Any }
  | LDeref lhs -> place (C.PDeref :: steps) lhs
  | LIndex (base, index) ->
    let%bind index = exp index in
    place (C.PIndex index :: steps) base
;;

let rec stmt = function
  | SLet (x, e) ->
    let%bind e = exp e in
    let%map x = fresh_var x in
    [ C.SLet (x, e) ]
  | SLetMut (x, e) ->
    let%bind e = exp e in
    let%map x = fresh_var x in
    [ C.SLetMut (x, e) ]
  | SFun (f, ps, tret, body) ->
    let%bind f =
      let%bind () =
        if Option.is_some (Builtin.of_ident f)
        then M.fail (Printf.sprintf "Function name %s is reserved." (name f))
        else (
          match%bind M.access funs >>| fun funs -> Map.find funs (name f) with
          | Some _ -> M.fail (Printf.sprintf "Function %s is already defined" (name f))
          | None -> M.return ())
      in
      let%bind f' = fresh f in
      let%map () =
        M.map_access funs ~f:(fun funs -> Map.set funs ~key:(name f) ~data:f')
      in
      f'
    in
    let%bind () =
      let param_name = function
        | ParamImm (x, _) | ParamMut (x, _) -> name x
      in
      let duplicate =
        ps
        |> List.fold_until
             ~init:(Set.empty (module String))
             ~f:(fun seen p ->
               let x = param_name p in
               if Set.mem seen x then Stop (Some x) else Continue (Set.add seen x))
             ~finish:(fun _ -> None)
      in
      match duplicate with
      | Some x -> M.fail (Printf.sprintf "Parameter %s is already defined" x)
      | None -> M.return ()
    in
    let%bind names = M.get in
    let%bind () = M.map_access vars ~f:(fun _ -> Map.empty (module String)) in
    let%bind ps =
      M.all
        (List.map ps ~f:(function
           | ParamImm (x, t) ->
             let%map x = fresh_var x in
             ParamImm (x, t)
           | ParamMut (x, t) ->
             let%map x = fresh_var x in
             ParamMut (x, t)))
    in
    let%bind body = nested body in
    let%bind body_names = M.get in
    let%map () = M.put { names with next = body_names.next } in
    [ C.SFun (f, ps, tret, body) ]
  | SAssign (lhs, rhs) ->
    let%bind lhs = place [] lhs in
    let%map rhs = exp rhs in
    [ C.SAssign (lhs, rhs) ]
  | SBlock body ->
    let%map body = nested body in
    [ C.SScope body ]
  | SIf (cond, body) ->
    let%bind cond = exp cond in
    let%map body = nested body in
    [ C.SIfElse (cond, body, []) ]
  | SIfElse (cond, t_body, f_body) ->
    let%bind cond = exp cond in
    let%bind t_body = nested t_body in
    let%map f_body = nested f_body in
    [ C.SIfElse (cond, t_body, f_body) ]
  | SWhile (cond, body) ->
    let%bind cond = exp cond in
    let%map body = nested body in
    [ C.SWhile (cond, body) ]
  | SReturn e ->
    let%map e = exp e in
    [ C.SReturn e ]
  | SExpr e ->
    let%map e = exp e in
    [ C.SExpr e ]

and stmt_list body =
  let rec stops = function
    | C.SReturn _ -> true
    | C.SScope body -> block_stops body
    | C.SIfElse (_, then_body, else_body) ->
      block_stops then_body && block_stops else_body
    | _ -> false
  and block_stops stmts =
    match List.last stmts with
    | Some stmt -> stops stmt
    | None -> false
  in
  match body with
  | [] -> M.return []
  | s :: rest ->
    let%bind lowered = stmt s in
    if List.exists lowered ~f:stops
    then M.return lowered
    else (
      let%map rest = stmt_list rest in
      lowered @ rest)

and nested body =
  let%bind names = M.get in
  let%bind body = stmt_list body in
  let%bind body_names = M.get in
  let%map () = M.put { names with next = body_names.next } in
  body
;;

let stmts stmts env = M.run (stmt_list stmts) env
