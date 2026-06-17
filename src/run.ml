open! Base
open Async
open AbsLang
module Borrowcheck = Passes.Borrowcheck
module Lowering = Passes.Lowering
module Interpreter = Passes.Interpreter
module Typecheck = Passes.Typecheck
module Value = Common.Value
module Env = Value.Env

type env =
  { names : Lowering.env
  ; tc : Typecheck.env
  ; bc : Borrowcheck.env
  ; interp : Interpreter.env
  ; line : int
  }

let empty_env : env =
  { names =
      { vars = Map.empty (module String); funs = Map.empty (module String); next = 0 }
  ; tc = { vars = Env.empty; muts = Env.empty; funs = Env.empty; ret = None }
  ; bc = { owns = Env.empty; graph = Common.Borrow_graph.empty }
  ; interp = { vars = Env.empty; funs = Env.empty; threads = []; error = Ivar.create () }
  ; line = 1
  }
;;

let parse (source : string) : (AbsLang.program, string) Result.t =
  let lexbuf = Lexing.from_string source in
  try Ok (ParLang.pProgram LexLang.token lexbuf) with
  | BNFC_Util.Parse_error (start_pos, end_pos) ->
    let line = start_pos.pos_lnum in
    let col_s = start_pos.pos_cnum - start_pos.pos_bol in
    let col_e = end_pos.pos_cnum - end_pos.pos_bol in
    Error (Printf.sprintf "Parse error at line %d, columns %d-%d" line col_s col_e)
  | Stdlib.Parsing.Parse_error -> Error "Parse error"
  | Failure msg -> Error ("Lexer error: " ^ msg)
;;

let run_line_async source ({ names; tc; bc; interp; line } : env) =
  let name_err = Result.map_error ~f:(fun e -> "Name error: " ^ e) in
  let tc_err = Result.map_error ~f:(fun e -> "Type error: " ^ e) in
  let bc_err = Result.map_error ~f:(fun e -> "Borrow error: " ^ e) in
  let rt_err = Result.map_error ~f:(fun e -> "Runtime error: " ^ e) in
  let checked =
    let open Result.Let_syntax in
    let%bind (Prog stmts) = parse source in
    let%bind stmts, names = Lowering.stmts stmts names |> name_err in
    let%bind stmts, tc = Typecheck.stmts stmts tc |> tc_err in
    let%bind _, bc' = Borrowcheck.stmts stmts bc |> bc_err in
    Ok (stmts, names, tc, bc')
  in
  match checked with
  | Error err -> Deferred.return (Error err)
  | Ok (stmts, names, tc, bc') ->
    let result =
      match stmts with
      | [ SExpr e ] ->
        Deferred.map (Interpreter.expression e interp) ~f:(fun result ->
          Result.map result ~f:(fun (v, interp) -> Some v, interp))
      | _ -> Interpreter.stmts stmts interp
    in
    Deferred.map result ~f:(fun result ->
      let open Result.Let_syntax in
      let%map v, interp = result |> rt_err in
      v, { names; tc; bc = bc'; interp; line = line + 1 })
;;

let run_line source env =
  Thread_safe.block_on_async_exn (fun () -> run_line_async source env)
;;

let check_line source ({ names; tc; bc; interp; line } : env) =
  let name_err = Result.map_error ~f:(fun e -> "Name error: " ^ e) in
  let tc_err = Result.map_error ~f:(fun e -> "Type error: " ^ e) in
  let bc_err = Result.map_error ~f:(fun e -> "Borrow error: " ^ e) in
  let open Result.Let_syntax in
  let%bind (Prog stmts) = parse source in
  let%bind stmts, names = Lowering.stmts stmts names |> name_err in
  let%bind stmts, tc = Typecheck.stmts stmts tc |> tc_err in
  let%map _, bc = Borrowcheck.stmts stmts bc |> bc_err in
  { names; tc; bc; interp; line = line + 1 }
;;

let run_async source =
  Deferred.map (run_line_async source empty_env) ~f:(fun result ->
    let open Result.Let_syntax in
    let%bind v_opt, _ = result in
    Result.of_option v_opt ~error:"Missing return")
;;

let run source = Thread_safe.block_on_async_exn (fun () -> run_async source)
