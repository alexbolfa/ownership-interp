open! Base
open Async
open Interp
module Value = Common.Value
module Out = Stdlib.Printf

let ansi code s = "\027[" ^ code ^ "m" ^ s ^ "\027[0m"
let red s = ansi "1;31" s
let cyan s = ansi "36" s

let print_file_error path err =
  Out.eprintf "%s %s\n%s %s\n%!" (red "error:") err (cyan "-->") path
;;

type loc =
  { line : int
  ; col : int
  }

let default_loc = { line = 1; col = 1 }

type source_token =
  { token : ParLang.token
  ; loc : loc
  }

type diagnostic_target =
  | Function_call of string
  | Mutable_borrow of string
  | Shared_borrow of string
  | Any_borrow of string
  | Assignment of string
  | Identifier_use of string
  | Deref_assignment
  | Deref_token
  | Index_token
  | List_literal
  | While_token
  | Function_definition of string option
  | Condition_expression
  | Bool_literal
  | Int_literal

let name_between err ~prefix ~suffix =
  err |> String.chop_prefix ~prefix |> Option.bind ~f:(String.chop_suffix ~suffix)
;;

let tokens source =
  let lexbuf = Lexing.from_string source in
  let token_start () =
    let pos = Lexing.lexeme_start_p lexbuf in
    { line = pos.pos_lnum; col = pos.pos_cnum - pos.pos_bol + 1 }
  in
  let rec loop acc =
    let token = LexLang.token lexbuf in
    let loc = token_start () in
    match token with
    | ParLang.TOK_EOF -> List.rev acc
    | _ -> loop ({ token; loc } :: acc)
  in
  try loop [] with
  | _ -> []
;;

let token_ident_name token =
  match token.token with
  | ParLang.TOK_Ident name -> Some name
  | _ -> None
;;

let token_is_ident token name =
  match token_ident_name token with
  | Some token_name -> String.equal token_name name
  | None -> false
;;

let is_statement_boundary token =
  match token.token with
  | ParLang.SYMB1 | ParLang.SYMB9 | ParLang.SYMB10 -> true
  | _ -> false
;;

let is_lvalue_boundary token =
  match token.token with
  | ParLang.SYMB1 | ParLang.SYMB12 | ParLang.SYMB10 -> true
  | _ -> false
;;

let find_first_loc ts ~f =
  List.find_map ts ~f:(fun token -> if f token then Some token.loc else None)
;;

let find_last_loc ts ~f =
  List.find_map (List.rev ts) ~f:(fun token -> if f token then Some token.loc else None)
;;

let token_at ts i = if i < 0 || i >= Array.length ts then None else Some ts.(i)

let locate_function_call ts function_name =
  Array.find_mapi ts ~f:(fun i token ->
    match token.token, token_at ts (i + 1), token_at ts (i - 1) with
    | ParLang.TOK_Ident name, Some { token = ParLang.SYMB5; _ }, previous
      when String.equal name function_name ->
      (match previous with
       | Some { token = ParLang.KW_fn; _ } -> None
       | _ -> Some token.loc)
    | _ -> None)
;;

let locate_function_definition ts function_name =
  Array.find_mapi ts ~f:(fun i token ->
    match token.token, token_at ts (i + 1) with
    | ParLang.KW_fn, Some ({ token = ParLang.TOK_Ident name; _ } as name_token)
      when Option.for_all function_name ~f:(String.equal name) -> Some name_token.loc
    | _ -> None)
;;

let locate_borrow ts name ~mutable_only ~shared_only =
  let found = ref None in
  Array.iteri ts ~f:(fun i token ->
    match token.token, token_at ts (i + 1) with
    | ParLang.SYMB4, Some next ->
      let is_mutable =
        match next.token with
        | ParLang.KW_mut -> true
        | _ -> false
      in
      if ((not mutable_only) || is_mutable) && ((not shared_only) || not is_mutable)
      then (
        let j = ref (if is_mutable then i + 2 else i + 1) in
        let hit = ref None in
        while
          Option.is_some (token_at ts !j)
          &&
          match token_at ts !j with
          | Some boundary when is_lvalue_boundary boundary -> false
          | _ -> true
        do
          (match token_at ts !j with
           | Some candidate when token_is_ident candidate name ->
             hit := Some candidate.loc
           | _ -> ());
          Int.incr j
        done;
        Option.iter !hit ~f:(fun loc -> found := Some loc))
    | _ -> ());
  !found
;;

let locate_assignment ts name =
  let found = ref None in
  Array.iteri ts ~f:(fun i token ->
    match token.token with
    | ParLang.SYMB7 ->
      let start = ref (i - 1) in
      while
        !start >= 0
        &&
        match token_at ts !start with
        | Some boundary -> not (is_statement_boundary boundary)
        | None -> false
      do
        Int.decr start
      done;
      let start = !start + 1 in
      let is_let =
        match token_at ts start with
        | Some { token = ParLang.KW_let; _ } -> true
        | _ -> false
      in
      if not is_let
      then
        for j = start to i - 1 do
          match token_at ts j with
          | Some candidate when token_is_ident candidate name ->
            found := Some candidate.loc
          | _ -> ()
        done
    | _ -> ());
  !found
;;

let locate_condition_expression ts =
  Array.find_mapi (Array.rev ts) ~f:(fun i token ->
    match token.token with
    | ParLang.KW_if | ParLang.KW_while ->
      let original_i = Array.length ts - i - 1 in
      Option.map (token_at ts (original_i + 1)) ~f:(fun token -> token.loc)
    | _ -> None)
;;

let locate_parse_error err =
  match String.chop_prefix err ~prefix:"Parse error at line " with
  | None -> None
  | Some rest ->
    (match String.lsplit2 rest ~on:',' with
     | None -> None
     | Some (line, cols) ->
       let col =
         cols
         |> String.strip
         |> String.chop_prefix ~prefix:"columns "
         |> Option.bind ~f:(fun cols -> String.lsplit2 cols ~on:'-')
         |> Option.map ~f:fst
       in
       (match Int.of_string_opt line, Option.bind col ~f:Int.of_string_opt with
        | Some line, Some col -> Some { line; col = col + 1 }
        | _ -> None))
;;

let diagnostic_target err =
  let named prefix suffix ctor = Option.map (name_between err ~prefix ~suffix) ~f:ctor in
  let borrow_lifetime =
    match String.chop_prefix err ~prefix:"Borrow error: Borrow of " with
    | None -> None
    | Some rest ->
      (match String.lsplit2 rest ~on:' ' with
       | Some (name, _)
         when String.is_substring rest ~substring:" does not live long enough for " ->
         Some (Any_borrow name)
       | _ -> None)
  in
  let function_expects =
    match String.chop_prefix err ~prefix:"Type error: Function " with
    | Some rest when String.is_substring rest ~substring:" expects " ->
      rest
      |> String.lsplit2 ~on:' '
      |> Option.map ~f:(fun (name, _) -> Function_call name)
    | _ -> None
  in
  List.find_map
    [ named "Name error: Function " " not defined" (fun name -> Function_call name)
    ; named "Name error: Variable " " is not bound" (fun name -> Identifier_use name)
    ; named "Type error: Cannot mutate immutable variable " "." (fun name ->
        Assignment name)
    ; function_expects
    ; named "Type error: Cannot borrow immutable variable " " as mutable." (fun name ->
        Mutable_borrow name)
    ; named
        "Borrow error: Cannot borrow "
        " as immutable because it is borrowed as mutable."
        (fun name -> Shared_borrow name)
    ; named
        "Borrow error: Cannot borrow "
        " as mutable because it is already borrowed."
        (fun name -> Mutable_borrow name)
    ; named "Borrow error: Cannot assign to " " because it is borrowed." (fun name ->
        Assignment name)
    ; named "Borrow error: Cannot move out of " " because it is borrowed." (fun name ->
        Identifier_use name)
    ; named "Borrow error: Value " " used after being moved." (fun name ->
        Identifier_use name)
    ; named
        "Borrow error: Cannot copy "
        " because it is borrowed as mutable."
        (fun name -> Identifier_use name)
    ; named
        "Borrow error: Cannot read "
        " because it is borrowed as mutable."
        (fun name -> Identifier_use name)
    ; borrow_lifetime
    ; named "Borrow error: While condition may move list variable " "." (fun _ ->
        While_token)
    ; named "Borrow error: While body may move list variable " "." (fun _ -> While_token)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Cannot mutate through immutable reference."
       then Some Deref_assignment
       else None)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Cannot borrow through immutable reference as mutable."
       then Some Deref_token
       else None)
    ; (if String.is_substring err ~substring:"Type error: Expected a list, but got"
       then Some Index_token
       else None)
    ; (if String.is_substring err ~substring:"Type error: Expected a reference, but got"
       then Some Deref_token
       else None)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Cannot infer element type of empty list literal"
       then Some List_literal
       else None)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Lists of references require explicit lifetimes."
       then Some List_literal
       else None)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Spawned threads cannot take reference arguments."
       then Some (Function_call "spawn")
       else None)
    ; named "Borrow error: Cannot mutate " " because it is borrowed." (fun name ->
        Assignment name)
    ; (if
         String.is_substring
           err
           ~substring:"Borrow error: Cannot move an owned value through a reference."
       then Some Deref_token
       else None)
    ; (if
         String.is_substring
           err
           ~substring:
             "Borrow error: Cannot move an owned value out of a list by indexing."
       then Some Index_token
       else None)
    ; (if
         String.is_substring
           err
           ~substring:"Type error: Expression should be of type bool"
       then Some Condition_expression
       else None)
    ; named "Type error: Function " " is missing a return." (fun name ->
        Function_definition (Some name))
    ; (if
         String.is_substring
           err
           ~substring:
             "Type error: Functions cannot return references without explicit lifetimes."
       then Some (Function_definition None)
       else None)
    ; (if
         String.is_substring err ~substring:"Type error: Expression should be of type"
         && String.is_substring err ~substring:"it has type bool"
       then Some Bool_literal
       else None)
    ; (if
         String.is_substring err ~substring:"Type error: Expression should be of type"
         && String.is_substring err ~substring:"it has type int"
       then Some Int_literal
       else None)
    ]
    ~f:Fn.id
;;

let diagnostic_loc source err =
  let ts = tokens source |> Array.of_list in
  let ts_list = Array.to_list ts in
  let located =
    match diagnostic_target err with
    | Some (Function_call name) -> locate_function_call ts name
    | Some (Mutable_borrow name) ->
      locate_borrow ts name ~mutable_only:true ~shared_only:false
    | Some (Shared_borrow name) ->
      locate_borrow ts name ~mutable_only:false ~shared_only:true
    | Some (Any_borrow name) ->
      locate_borrow ts name ~mutable_only:false ~shared_only:false
    | Some (Assignment name) -> locate_assignment ts name
    | Some (Identifier_use name) ->
      find_last_loc ts_list ~f:(fun token -> token_is_ident token name)
    | Some Deref_assignment ->
      find_last_loc ts_list ~f:(function
        | { token = ParLang.SYMB13; _ } -> true
        | _ -> false)
    | Some Deref_token ->
      find_last_loc ts_list ~f:(function
        | { token = ParLang.SYMB13; _ } -> true
        | _ -> false)
    | Some Index_token | Some List_literal ->
      find_last_loc ts_list ~f:(function
        | { token = ParLang.SYMB2; _ } -> true
        | _ -> false)
    | Some While_token ->
      find_first_loc ts_list ~f:(function
        | { token = ParLang.KW_while; _ } -> true
        | _ -> false)
    | Some (Function_definition name) -> locate_function_definition ts name
    | Some Condition_expression -> locate_condition_expression ts
    | Some Bool_literal ->
      find_last_loc ts_list ~f:(function
        | { token = ParLang.KW_True | ParLang.KW_False; _ } -> true
        | _ -> false)
    | Some Int_literal ->
      find_last_loc ts_list ~f:(function
        | { token = ParLang.TOK_Integer _; _ } -> true
        | _ -> false)
    | None -> locate_parse_error err
  in
  Option.value located ~default:default_loc
;;

let print_check_error path source err =
  let { line; col } = diagnostic_loc source err in
  Out.eprintf "%s:%d:%d: %s\n%!" path line col err
;;

let rec repl env =
  match LNoise.linenoise "> " with
  | None -> Out.printf "Goodbye!\n"
  | Some line ->
    ignore (LNoise.history_add line : (unit, string) Result.t);
    let env' =
      try
        match Run.run_line line env with
        | Ok (v_opt, env) ->
          Option.iter v_opt ~f:(fun v -> Out.printf "%s\n%!" (Value.to_string v));
          env
        | Error err ->
          Out.printf "%s\n%!" err;
          env
      with
      | exn ->
        (match Monitor.extract_exn exn with
         | Stdlib.Sys.Break ->
           Out.printf "\nGoodbye!\n%!";
           Stdlib.exit 130
         | exn -> raise exn)
    in
    repl env'
;;

let run_file_async path source =
  Deferred.bind (Run.run_line_async source Run.empty_env) ~f:(function
    | Ok (v_opt, _) ->
      Option.iter v_opt ~f:(fun v -> Out.printf "%s\n%!" (Value.to_string v));
      Deferred.unit
    | Error err ->
      print_file_error path err;
      Stdlib.exit 1)
;;

let check_file path source =
  match Run.check_line source Run.empty_env with
  | Ok _ -> ()
  | Error err ->
    print_check_error path source err;
    Stdlib.exit 1
;;

let read_file path =
  let ic = Stdlib.open_in path in
  let n = Stdlib.in_channel_length ic in
  let source = Stdlib.really_input_string ic n in
  Stdlib.close_in ic;
  source
;;

let () =
  Stdlib.Sys.catch_break true;
  match Stdlib.Sys.argv with
  | [| _ |] ->
    (try repl Run.empty_env with
     | Stdlib.Sys.Break -> Out.printf "\nGoodbye!\n")
  | [| _; "--check"; path |] ->
    let source = read_file path in
    check_file path source
  | [| _; path |] ->
    let source = read_file path in
    (match
       Thread_safe.block_on_async_exn (fun () ->
         Monitor.try_with ~extract_exn:true (fun () -> run_file_async path source))
     with
     | Ok () -> ()
     | Error Stdlib.Sys.Break ->
       Out.printf "\nInterrupted\n%!";
       Stdlib.exit 130
     | Error exn -> raise exn)
  | _ ->
    Out.eprintf "Usage: %s [--check FILE | FILE]\n" Stdlib.Sys.argv.(0);
    Stdlib.exit 1
;;
