open! Base
open AbsLang
module C = Core
module Borrow_graph = Common.Borrow_graph
open Borrow_graph
module Value = Common.Value
module Env = Value.Env

type ownership =
  | Live
  | Moved

type access_mode =
  | Read
  | Move

type borrow = Borrow_graph.target

type borrow_source =
  { borrow : borrow
  ; owner : ident option
  }

type env =
  { owns : ownership Env.t
  ; graph : Borrow_graph.t
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

let unreachable msg =
  failwith (Printf.sprintf "internal error (borrowchecker bug): %s" msg)
;;

let map_owns f = M.map_access owns ~f
let map_graph f = M.map_access graph ~f

let type_of (e : C.exp) =
  match e.typ with
  | C.Type t -> t
  | C.Any -> unreachable "expected typed core expression"
;;

let type_of_place place =
  match place.C.typ with
  | C.Type t -> t
  | C.Any -> unreachable "expected typed core place"
;;

let self_borrow x = function
  | TRef _ -> Some { root = x; kind = Value.Shared }
  | TMutRef _ -> Some { root = x; kind = Value.Mutable }
  | TInt | TBool | TUnit | TList _ -> None
;;

let merge_owns a b =
  a
  |> Env.fold ~init:Env.empty ~f:(fun merged name oa ->
    let id = Ident name in
    match oa, Env.find id b with
    | Live, Some Live -> Env.bind id Live merged
    | _ -> Env.bind id Moved merged)
;;

let active_borrows ?(ignore = []) root =
  let%map env = M.get in
  Borrow_graph.active ~ignore root env.graph
;;

let require_unborrowed ?(ignore = []) x action =
  match%bind active_borrows ~ignore x with
  | [] -> M.return ()
  | _ ->
    M.fail (Printf.sprintf "Cannot %s %s because it is borrowed." action (Value.name x))
;;

let require_not_mutably_borrowed x action =
  match%bind active_borrows x with
  | borrows when List.exists borrows ~f:(fun b -> Value.equal_ref_kind b.kind Mutable) ->
    M.fail
      (Printf.sprintf
         "Cannot %s %s because it is borrowed as mutable."
         action
         (Value.name x))
  | _ -> M.return ()
;;

let require_live_owned x owned =
  if not owned
  then M.return ()
  else (
    match%bind M.access owns >>| Env.find x with
    | Some Live -> M.return ()
    | Some Moved ->
      M.fail (Printf.sprintf "Value %s used after being moved." (Value.name x))
    | None -> unreachable "bound variable missing ownership")
;;

let require_live_var x typ = require_live_owned x (Value.type_is_owned typ)

let bind_var x typ =
  if Value.type_is_owned typ then map_owns (Env.bind x Live) else M.return ()
;;

let drop_ref x = map_graph (Borrow_graph.remove_owner x)
let index_protect_owner = Ident "\000index-protect"

let with_index_root_protection root m =
  let%bind already_protected =
    M.access graph
    >>| fun graph -> not (List.is_empty (Borrow_graph.owned_by index_protect_owner graph))
  in
  let%bind () =
    map_graph
      (Borrow_graph.add_owned ~owner:index_protect_owner { root; kind = Value.Shared })
  in
  let%bind v = m in
  let%map () =
    if already_protected
    then M.return ()
    else map_graph (Borrow_graph.remove_owner index_protect_owner)
  in
  v
;;

let expr_borrow_sources e =
  let rec refs e =
    match e.C.node with
    | C.EBorrow (kind, place) ->
      M.return [ { borrow = { root = place.root; kind }; owner = None } ]
    | C.EVar x when Value.type_is_ref (type_of e) ->
      let%map env = M.get in
      let borrows =
        match Borrow_graph.owned_by x env.graph with
        | [] -> Option.to_list (self_borrow x (type_of e))
        | borrows -> borrows
      in
      List.map borrows ~f:(fun borrow -> { borrow; owner = Some x })
    | C.EIf (_, t, f) ->
      let%bind t = refs t in
      let%map f = refs f in
      t @ f
    | _ -> M.return []
  in
  refs e
;;

let ref_borrows x typ =
  if Value.type_is_ref typ
  then (
    let%map env = M.get in
    match Borrow_graph.owned_by x env.graph with
    | [] -> Option.to_list (self_borrow x typ)
    | borrows -> borrows)
  else M.return []
;;

let rec check_place_use ?(protect_index_root = false) place =
  let root_typ =
    match place.C.root_typ with
    | C.Type t -> t
    | C.Any -> unreachable "expected typed core place root"
  in
  let%bind () = require_live_var place.C.root root_typ in
  let index_checks =
    List.filter_map place.C.steps ~f:(function
      | C.PIndex index -> Some (check_exp Move index)
      | C.PDeref -> None)
  in
  if protect_index_root && not (List.is_empty index_checks)
  then with_index_root_protection place.C.root (M.all_unit index_checks)
  else M.all_unit index_checks

and protected_index_roots e =
  match e.C.node with
  | C.EVar x -> M.return [ x ]
  | C.EIndex (base, _) -> protected_index_roots base
  | C.EDeref inner ->
    let%bind inner_roots = protected_index_roots inner in
    let%map sources = expr_borrow_sources inner in
    inner_roots @ List.map sources ~f:(fun { borrow; owner = _ } -> borrow.root)
  | C.EIf (_, t, f) ->
    let%bind t_roots = protected_index_roots t in
    let%map f_roots = protected_index_roots f in
    t_roots @ f_roots
  | C.EInt _
  | C.ETrue
  | C.EFalse
  | C.EMul _
  | C.EDiv _
  | C.EAdd _
  | C.EAnd _
  | C.EEq _
  | C.ELt _
  | C.EBorrow _
  | C.ECall _
  | C.ESpawn _
  | C.EYield
  | C.EList _
  | C.EListMutate _ -> M.return []

and protected_read_roots e =
  match e.C.node with
  | C.EVar x when Value.type_is_owned (type_of e) -> M.return [ x ]
  | C.EIndex (base, _) -> protected_index_roots base
  | C.EIf (_, t, f) ->
    let%bind t_roots = protected_read_roots t in
    let%map f_roots = protected_read_roots f in
    t_roots @ f_roots
  | C.EInt _
  | C.ETrue
  | C.EFalse
  | C.EMul _
  | C.EDiv _
  | C.EAdd _
  | C.EAnd _
  | C.EEq _
  | C.ELt _
  | C.EBorrow _
  | C.EDeref _
  | C.ECall _
  | C.ESpawn _
  | C.EYield
  | C.EList _
  | C.EListMutate _
  | C.EVar _ -> M.return []

and check_exp mode (e : C.exp) =
  match mode, e.node with
  | Read, C.EVar x ->
    let typ = type_of e in
    let%bind () = require_live_var x typ in
    require_not_mutably_borrowed x "read"
  | Move, C.EVar x ->
    let typ = type_of e in
    let owned = Value.type_is_owned typ in
    let%bind moved_ref_borrows = ref_borrows x typ in
    let%bind () = require_live_owned x owned in
    let%bind () = if owned then require_unborrowed x "move out of" else M.return () in
    if owned
    then (
      let%bind () =
        M.all_unit
          (List.map moved_ref_borrows ~f:(fun borrow ->
             match%bind active_borrows ~ignore:[ x ] borrow.root with
             | [] -> M.return ()
             | _ ->
               M.fail
                 (Printf.sprintf
                    "Cannot move out of %s because it is borrowed."
                    (Value.name x))))
      in
      let%bind () =
        map_graph (fun graph ->
          List.fold moved_ref_borrows ~init:graph ~f:(fun graph borrow ->
            Borrow_graph.add_temporary borrow graph))
      in
      let%bind () = map_owns (Env.bind x Moved) in
      drop_ref x)
    else (
      let%bind () = require_not_mutably_borrowed x "copy" in
      if Value.type_is_ref typ
      then
        map_graph (fun graph ->
          List.fold moved_ref_borrows ~init:graph ~f:(fun graph borrow ->
            Borrow_graph.add_temporary { borrow with kind = Value.Shared } graph))
      else M.return ())
  | _, C.EBorrow (kind, place) ->
    let%bind () = check_place_use ~protect_index_root:true place in
    let%bind () =
      match%bind active_borrows place.C.root with
      | [] -> M.return ()
      | borrows ->
        (match
           ( kind
           , List.exists borrows ~f:(fun borrow ->
               Value.equal_ref_kind borrow.kind Mutable) )
         with
         | Value.Shared, false -> M.return ()
         | Value.Shared, true ->
           M.fail
             (Printf.sprintf
                "Cannot borrow %s as immutable because it is borrowed as mutable."
                (Value.name place.C.root))
         | Value.Mutable, _ ->
           M.fail
             (Printf.sprintf
                "Cannot borrow %s as mutable because it is already borrowed."
                (Value.name place.C.root)))
    in
    map_graph (Borrow_graph.add_temporary { root = place.root; kind })
  | _, C.EDeref inner ->
    let%bind sources = expr_borrow_sources inner in
    let%bind () =
      M.all_unit
        (List.map sources ~f:(fun { borrow; owner } ->
           let ignore = Option.to_list owner in
           match%bind active_borrows ~ignore borrow.root with
           | borrows
             when List.exists borrows ~f:(fun borrow ->
                    Value.equal_ref_kind borrow.kind Mutable) ->
             let name = Option.value owner ~default:borrow.root in
             M.fail
               (Printf.sprintf
                  "Cannot read %s because it is borrowed as mutable."
                  (Value.name name))
           | _ -> M.return ()))
    in
    let%bind () = check_exp Read inner in
    if Poly.(mode = Move) && Value.type_is_owned (type_of e)
    then M.fail "Cannot move an owned value through a reference."
    else M.return ()
  | Read, C.EIndex (base, index) ->
    let%bind roots = protected_index_roots base in
    let%bind () = check_exp Read base in
    let%bind () =
      map_graph (fun graph ->
        List.fold roots ~init:graph ~f:(fun graph root ->
          Borrow_graph.add_temporary { root; kind = Value.Shared } graph))
    in
    check_exp Move index
  | Move, C.EIndex _ ->
    if Value.type_is_owned (type_of e)
    then M.fail "Cannot move an owned value out of a list by indexing."
    else check_exp Read e
  | _, C.EInt _ | _, C.ETrue | _, C.EFalse | _, C.EYield -> M.return ()
  | _, C.EMul (a, b)
  | _, C.EDiv (a, b)
  | _, C.EAdd (a, b)
  | _, C.EAnd (a, b)
  | _, C.ELt (a, b) ->
    let%bind () = check_exp Move a in
    check_exp Move b
  | _, C.EEq (a, b) ->
    let%bind () = check_exp Read a in
    let%bind roots = protected_read_roots a in
    let%bind () =
      map_graph (fun graph ->
        List.fold roots ~init:graph ~f:(fun graph root ->
          Borrow_graph.add_temporary { root; kind = Value.Shared } graph))
    in
    check_exp Read b
  | _, C.EIf (cond, t, f) ->
    let%bind () = check_exp Move cond in
    let%bind before = M.get in
    let%bind () = check_exp mode t in
    let%bind t_env = M.get in
    let%bind () = M.put before in
    let%bind () = check_exp mode f in
    let%bind f_env = M.get in
    M.put
      { owns = merge_owns t_env.owns f_env.owns
      ; graph = Borrow_graph.merge t_env.graph f_env.graph
      }
  | _, C.EListMutate (xs, op) ->
    let%bind () = require_live_var xs (TList TUnit) in
    let%bind () = require_unborrowed xs "mutate" in
    with_index_root_protection
      xs
      (match op with
       | C.Push x -> check_exp Move x
       | C.Insert (i, x) ->
         let%bind () = check_exp Move i in
         check_exp Move x
       | C.Remove i -> check_exp Move i)
  | _, C.ECall (_, args) | _, C.ESpawn (_, args) ->
    M.all_unit (List.map args ~f:(check_exp Move))
  | _, C.EList es -> M.all_unit (List.map es ~f:(check_exp Move))

and block_env body =
  let keep_outer ~outer ~inner =
    let owns =
      Env.fold outer.owns ~init:Env.empty ~f:(fun owns name outer_ownership ->
        let id = Ident name in
        Env.bind id (Option.value (Env.find id inner.owns) ~default:outer_ownership) owns)
    in
    { owns; graph = Borrow_graph.keep_outer ~outer:outer.graph ~inner:inner.graph }
  in
  let%bind () = M.scoped ~enter:Fn.id ~leave:keep_outer (check_stmts body) in
  M.get

and stmt stmt =
  let action =
    match stmt with
    | C.SLet (x, e) | C.SLetMut (x, e) ->
      let%bind sources = expr_borrow_sources e in
      let borrows = List.map sources ~f:(fun source -> source.borrow) in
      let%bind () = check_exp Move e in
      let%bind () = bind_var x (type_of e) in
      if Value.type_is_ref (type_of e)
      then (
        let%bind () = drop_ref x in
        map_graph (Borrow_graph.add_owned_all ~owner:x borrows))
      else M.return ()
    | C.SFun (_f, params, _tret, body) ->
      let enter _env = { owns = Env.empty; graph = Borrow_graph.empty } in
      let leave ~outer ~inner:_ = outer in
      M.scoped
        ~enter
        ~leave
        (let%bind () =
           M.all_unit
             (List.map params ~f:(fun p ->
                bind_var (Value.param_ident p) (Value.param_type p)))
         in
         check_stmts body)
    | C.SAssign (place, rhs) ->
      let direct = List.is_empty place.steps in
      let owner_write =
        List.for_all place.steps ~f:(function
          | C.PIndex _ -> true
          | C.PDeref -> false)
      in
      let%bind () = if direct then M.return () else check_place_use place in
      let%bind () =
        if owner_write then require_unborrowed place.root "assign to" else M.return ()
      in
      let%bind () = check_exp Move rhs in
      let%bind () =
        if owner_write
        then M.return ()
        else (
          let root_typ =
            match place.C.root_typ with
            | C.Type t -> t
            | C.Any -> unreachable "expected typed core place root"
          in
          let%bind borrows = ref_borrows place.root root_typ in
          M.all_unit
            (List.map borrows ~f:(fun borrow ->
               require_unborrowed ~ignore:[ place.root ] borrow.root "assign to")))
      in
      let%bind () =
        if direct
        then (
          let%bind () =
            if Value.type_is_owned (type_of_place place)
            then map_owns (Env.bind place.root Live)
            else M.return ()
          in
          if Value.type_is_ref (type_of_place place)
          then M.return ()
          else drop_ref place.root)
        else M.return ()
      in
      M.return ()
    | C.SScope body ->
      let%map _ = block_env body in
      ()
    | C.SIfElse (cond, t_body, f_body) ->
      let%bind () = check_exp Move cond in
      let%bind before = M.get in
      let%bind t_env = block_env t_body in
      let%bind () = M.put before in
      let%bind f_env = block_env f_body in
      M.put
        { owns = merge_owns t_env.owns f_env.owns
        ; graph =
            Borrow_graph.set_temporaries
              (Borrow_graph.merge_stored t_env.graph f_env.graph)
              (Borrow_graph.temporaries before.graph)
        }
    | C.SWhile (cond, body) ->
      let moved_between before env =
        Env.fold before.owns ~init:[] ~f:(fun acc name ownership ->
          (name, ownership) :: acc)
        |> List.find_map ~f:(fun (name, before_ownership) ->
          let id = Ident name in
          match before_ownership, Env.find id env.owns with
          | Live, Some Moved -> Some name
          | _ -> None)
      in
      let%bind before = M.get in
      let%bind () = check_exp Move cond in
      let%bind cond_env = M.get in
      (match moved_between before cond_env with
       | Some name ->
         M.fail
           (Printf.sprintf
              "While condition may move list variable %s."
              (Value.name (Ident name)))
       | None ->
         let%bind () = M.put before in
         let%bind body_env = block_env body in
         (match moved_between before body_env with
          | Some name ->
            M.fail
              (Printf.sprintf
                 "While body may move list variable %s."
                 (Value.name (Ident name)))
          | None ->
            M.put
              { before with
                graph =
                  Borrow_graph.set_temporaries
                    (Borrow_graph.merge_stored before.graph body_env.graph)
                    (Borrow_graph.temporaries before.graph)
              }))
    | C.SReturn e -> check_exp Move e
    | C.SExpr e -> check_exp Read e
  in
  let%bind () = action in
  map_graph Borrow_graph.clear_temporaries

and check_stmts body = M.all_unit (List.map body ~f:stmt)

let stmts body env = M.run (check_stmts body) env
