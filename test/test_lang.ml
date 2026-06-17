open! Base
open Interp
module Borrowcheck = Passes.Borrowcheck
module Lowering = Passes.Lowering
module Typecheck = Passes.Typecheck
module Value = Common.Value

let empty_names : Lowering.env =
  { vars = Map.empty (module String); funs = Map.empty (module String); next = 0 }
;;

let eval source =
  match Run.run source with
  | Ok v -> Stdio.printf "%s\n" (Value.to_string v)
  | Error err -> Stdio.printf "%s\n" err
;;

let typecheck source =
  let result =
    let open Result.Let_syntax in
    let%bind (Prog stmts) = Run.parse source in
    let%bind stmts, _ =
      Result.map_error (Lowering.stmts stmts empty_names) ~f:(fun e -> "Name error: " ^ e)
    in
    let%bind stmts, _ =
      Result.map_error (Typecheck.stmts stmts Run.empty_env.tc) ~f:(fun e ->
        "Type error: " ^ e)
    in
    let%bind _, _ =
      Result.map_error (Borrowcheck.stmts stmts Run.empty_env.bc) ~f:(fun e ->
        "Borrow error: " ^ e)
    in
    Ok ()
  in
  match result with
  | Ok () -> Stdio.printf "ok\n"
  | Error err -> Stdio.printf "%s\n" err
;;

let repl lines =
  let rec loop env = function
    | [] -> ()
    | line :: lines ->
      (match Run.run_line line env with
       | Ok (v_opt, env) ->
         Option.iter v_opt ~f:(fun v -> Stdio.printf "%s\n" (Value.to_string v));
         loop env lines
       | Error err -> Stdio.printf "%s\n" err)
  in
  loop Run.empty_env lines
;;

let%expect_test "repl prints single expressions" =
  repl [ "let x = 2"; "x" ];
  [%expect {| 2 |}];
  repl [ "let xs = [1, 2, 3]"; "xs"; "xs" ];
  [%expect
    {|
    [1, 2, 3]
    [1, 2, 3] |}];
  eval "let x = 2; x";
  [%expect {| Missing return |}]
;;

let%expect_test "arithmetic & control flow" =
  (* ok: arithmetic with precedence *)
  eval "return 2 + 3 * 4";
  [%expect {| 14 |}];
  eval "return 10 * 5 / 2";
  [%expect {| 25 |}];
  (* ok: if/then/else *)
  eval "return if 3 < 5 then 1 else 2";
  [%expect {| 1 |}];
  (* ok: comparison surface forms are lowered to the smaller core comparison set *)
  eval "return if 3 > 2 && 3 >= 3 && 2 <= 3 then 1 else 0";
  [%expect {| 1 |}];
  (* ok: not and subtraction are lowered out of core *)
  eval "return if !False then 10 - 3 else 0";
  [%expect {| 7 |}];
  (* ok: boolean operators short-circuit *)
  eval "return True || 1 / 0 == 0";
  [%expect {| true |}];
  eval "return False && 1 / 0 == 0";
  [%expect {| false |}];
  (* ok: non-final block expression is discarded *)
  eval "if True then { 1 }; return 2";
  [%expect {| 2 |}];
  (* fails: division by zero at runtime *)
  eval "return 10 / 0";
  [%expect {| Runtime error: division by zero |}]
;;

let%expect_test "bindings & mutation" =
  (* ok: mutable variable reassignment *)
  eval "let mut x = 1; x = x + 10; return x";
  [%expect {| 11 |}];
  (* fails: assignment to immutable binding *)
  typecheck "let x = 1; x = 2; x";
  [%expect {| Type error: Cannot mutate immutable variable x. |}];
  (* fails: assignment of wrong type *)
  typecheck "let mut x = 1; x = True; x";
  [%expect
    {| Type error: Expression should be of type int, but it has type bool instead. |}]
;;

let%expect_test "blocks" =
  (* ok: block-local binding does not escape *)
  eval "let mut x = 1; { let y = 5; x = x + y }; return x";
  [%expect {| 6 |}];
  (* ok: mutation to outer mutable variable persists *)
  eval "let mut x = 10; { x = 99 }; return x";
  [%expect {| 99 |}];
  (* fails: block-local binding is not visible after the block *)
  typecheck "let mut x = 1; { let y = 5 }; y";
  [%expect {| Name error: Variable y is not bound |}];
  (* fails: cannot assign to outer immutable binding from inside a block *)
  typecheck "let x = 1; { x = 2 }; x";
  [%expect {| Type error: Cannot mutate immutable variable x. |}];
  (* ok: nested blocks *)
  eval "let mut x = 0; { let a = 1; { let b = 2; x = a + b } }; return x";
  [%expect {| 3 |}];
  (* ok: local mut shadow with assignment leaves outer untouched *)
  eval "let mut x = 1; { let mut x = 99; x = 100 }; return x";
  [%expect {| 1 |}]
;;

let%expect_test "if statements" =
  (* ok: if-then taken *)
  eval "let mut x = 0; if True then { x = 1 }; return x";
  [%expect {| 1 |}];
  (* ok: if-then skipped *)
  eval "let mut x = 0; if False then { x = 1 }; return x";
  [%expect {| 0 |}];
  (* ok: if-then-else, then branch *)
  eval "let mut x = 0; if 3 < 5 then { x = 1 } else { x = 2 }; return x";
  [%expect {| 1 |}];
  (* ok: if-then-else, else branch *)
  eval "let mut x = 0; if 3 > 5 then { x = 1 } else { x = 2 }; return x";
  [%expect {| 2 |}];
  (* fails: condition must be bool *)
  typecheck "if 1 then { }";
  [%expect
    {| Type error: Expression should be of type bool, but it has type int instead. |}];
  (* fails: branch-local binding does not escape *)
  typecheck "if True then { let y = 1 }; y";
  [%expect {| Name error: Variable y is not bound |}]
;;

let%expect_test "while loops" =
  (* ok: countdown accumulator *)
  eval "let mut i = 0; let mut s = 0; while i < 5 { s = s + i; i = i + 1 }; return s";
  [%expect {| 10 |}];
  (* ok: never enters when false *)
  eval "let mut x = 7; while False { x = 0 }; return x";
  [%expect {| 7 |}];
  (* fails: condition must be bool *)
  typecheck "while 1 { }";
  [%expect
    {| Type error: Expression should be of type bool, but it has type int instead. |}];
  (* fails: loop-local binding does not escape *)
  typecheck "let mut i = 0; while i < 1 { let y = 1; i = i + 1 }; y";
  [%expect {| Name error: Variable y is not bound |}]
;;

let%expect_test "functions" =
  (* ok: call as expression inside arithmetic *)
  eval "fn double (x : int) -> int { return x * 2 }; return double(3) + double(4)";
  [%expect {| 14 |}];
  (* ok: binding a call result with let *)
  eval "fn inc (x : int) -> int { return x + 1 }; let r = inc(41); return r";
  [%expect {| 42 |}];
  (* ok: inferring the call's return type *)
  typecheck "fn isZero (x : int) -> bool { return x == 0 }; return isZero(42)";
  [%expect {| ok |}];
  (* fails: call with wrong argument type *)
  typecheck "fn increment (x : int) -> int { return x + 1 }; return increment(True)";
  [%expect
    {| Type error: Expression should be of type int, but it has type bool instead. |}];
  (* fails: call to undefined function *)
  typecheck "noSuchFn(1)";
  [%expect {| Name error: Function noSuchFn not defined |}];
  (* fails: function body referencing an outer variable *)
  typecheck "let x = 1; fn f (y : int) -> int { x + y }; f(2)";
  [%expect {| Name error: Variable x is not bound |}];
  (* ok: recursive function definition *)
  eval
    "fn fact (n : int) -> int { return if n == 0 then 1 else n * fact(n - 1) }; return \
     fact(5)";
  [%expect {| 120 |}];
  (* fails: duplicate function definition *)
  typecheck
    "fn f (x : int) -> int { return x }; fn f (x : int) -> int { return x + 1 }; return \
     f(0)";
  [%expect {| Name error: Function f is already defined |}];
  (* fails: duplicate parameter names would otherwise silently shadow *)
  typecheck "fn f (x : int, x : int) -> int { return x }; return f(1, 2)";
  [%expect {| Name error: Parameter x is already defined |}];
  typecheck "fn f (x : int, mut x : int) -> int { x = x + 1; return x }; return f(1, 2)";
  [%expect {| Name error: Parameter x is already defined |}];
  (* ok: zero-parameter function *)
  eval "fn answer () -> int { return 42 }; return answer()";
  [%expect {| 42 |}];
  (* ok: multi-parameter function *)
  eval
    "fn add3 (a : int, b : int, c : int) -> int { return a + b + c }; return add3(1, 2, \
     3)";
  [%expect {| 6 |}];
  (* ok: statement body with local bindings *)
  eval "fn f (x : int) -> int { let y = x * 2; let z = y + 1; return z }; return f(10)";
  [%expect {| 21 |}];
  (* ok: statement body using while *)
  eval
    "fn sum_to (n : int) -> int { let mut i = 0; let mut s = 0; while i <= n { s = s + \
     i; i = i + 1 }; return s }; return sum_to(5)";
  [%expect {| 15 |}];
  (* fails: wrong arity *)
  typecheck "fn add (a : int, b : int) -> int { return a + b }; return add(1)";
  [%expect {| Type error: Function add expects 2 argument(s), but was given 1. |}];
  (* fails: missing return *)
  typecheck "fn nope (x : int) -> int { let y = x }; nope(1)";
  [%expect {| Type error: Function nope is missing a return. |}];
  (* ok: function body ends in return *)
  eval
    "fn two_or_thirteen (x : bool) -> int { return if x then 2 else 13 }; return \
     two_or_thirteen(True)";
  [%expect {| 2 |}];
  eval
    "fn two_or_thirteen (x : bool) -> int { return if x then 2 else 13 }; return \
     two_or_thirteen(False)";
  [%expect {| 13 |}];
  (* fails: if-else branches with mismatched return types *)
  typecheck
    "fn bad (x : bool) -> int { if x then { return 1 } else { return True } }; return \
     bad(True)";
  [%expect
    {| Type error: Expression should be of type int, but it has type bool instead. |}];
  (* ok: both if-else branches return, so the function definitely returns *)
  eval
    "fn branchy (x : bool) -> int { if x then { return 1 } else { return 2 } }; return \
     branchy(False)";
  [%expect {| 2 |}];
  (* ok: unreachable statements after return are dropped during lowering *)
  eval "fn early () -> int { return 1; True }; return early()";
  [%expect {| 1 |}];
  (* ok: mutable parameter can be reassigned inside the body *)
  eval "fn bump (mut x : int) -> int { x = x + 1; return x }; return bump(41)";
  [%expect {| 42 |}];
  (* ok: mutating a mut param does not affect the caller's binding *)
  eval
    "fn bump (mut x : int) -> int { x = x + 1; return x }; let mut y = 10; let r = \
     bump(y); return r + y";
  [%expect {| 21 |}];
  (* ok: mix of mut and immutable params *)
  eval
    "fn shift (mut a : int, b : int) -> int { a = a + b; return a }; return shift(5, 7)";
  [%expect {| 12 |}];
  (* fails: cannot reassign an immutable parameter *)
  typecheck "fn nope (x : int) -> int { x = x + 1; x }; nope(0)";
  [%expect {| Type error: Cannot mutate immutable variable x. |}]
;;

let%expect_test "lists" =
  (* ok: list mutation through index assignment and builtins *)
  eval
    "let mut xs = [1, 2, 3]; xs[0] = 10; push(xs, 4); insert(xs, 0, 0); remove(xs, 1); \
     return xs";
  [%expect {| [0, 2, 3, 4] |}];
  (* ok: mutating builtins return unit, not another owner of the list *)
  eval "let mut xs = [1]; return push(xs, 2)";
  [%expect {| () |}];
  typecheck "let mut xs = [1]; let ys = push(xs, 2); return ys[0]";
  [%expect {| Type error: Expected a list, but got unit |}];
  eval "fn add_one (mut xs : [int]) -> unit { return push(xs, 1) }; return add_one([0])";
  [%expect {| () |}];
  (* ok: function return type provides empty-list context *)
  eval "fn empty () -> [int] { return [] }; return empty()";
  [%expect {| [] |}];
  (* fails: list builtins require a mutable variable *)
  typecheck "let xs = [1, 2]; push(xs, 3)";
  [%expect {| Type error: Cannot mutate immutable variable xs. |}];
  (* fails: list mutation arguments cannot move or mutably borrow the target list *)
  typecheck
    "fn take (xs : [int]) -> int { return 0 }; let mut xs = [1]; insert(xs, take(xs), \
     2); return 0";
  [%expect {| Borrow error: Cannot move out of xs because it is borrowed. |}];
  typecheck
    "fn val (xs : &mut [[int]]) -> [int] { (*xs)[0] = [9]; return [0] }; let mut xss = \
     [[1]]; push(xss, val(&mut xss)); return 0";
  [%expect
    {| Borrow error: Cannot borrow xss as mutable because it is already borrowed. |}]
;;

let%expect_test "references" =
  (* ok: immutable borrow and dereference *)
  eval "let a = 21; let b = &a; return *b";
  [%expect {| 21 |}];
  (* ok: mutable borrow can write through dereference *)
  eval "let mut a = 21; { let b = &mut a; *b = *b + *b }; return a";
  [%expect {| 42 |}];
  (* ok: references can point at list elements *)
  eval "let mut xs = [1, 2]; { let r = &mut xs[0]; *r = 9 }; return xs[0]";
  [%expect {| 9 |}];
  (* ok: mutable references can mutate indexed list places through dereference *)
  eval
    "fn set_first (xs : &mut [int]) -> int { (*xs)[0] = 7; return (*xs)[0] }; let mut xs \
     = [1, 2]; return set_first(&mut xs)";
  [%expect {| 7 |}];
  (* ok: functions can read through shared references *)
  eval "fn get (x : &int) -> int { return *x }; let a = 5; return get(&a)";
  [%expect {| 5 |}];
  (* ok: functions can write through mutable references *)
  eval
    "fn set (x : &mut int) -> int { *x = 7; return 0 }; let mut a = 1; set(&mut a); \
     return a";
  [%expect {| 7 |}];
  (* ok: a mutable borrow can be used where a shared borrow is expected *)
  eval
    "fn get (x : &int) -> int { return *x }; let mut a = 5; let b = &mut a; return get(b)";
  [%expect {| 5 |}];
  (* fails: if-expression joins do not coerce between shared and mutable references *)
  typecheck "let mut a = 1; let r = if True then &a else &mut a; return *r";
  [%expect
    {| Type error: Expression should be of type & int, but it has type & mut int instead. |}];
  typecheck "let mut a = 1; let r = if True then &mut a else &a; return *r";
  [%expect
    {| Type error: Expression should be of type & mut int, but it has type & int instead. |}];
  (* ok: shared use of a mutable reference lasts only for that statement *)
  eval
    "fn get (x : &int) -> int { return *x }; let mut n = 1; let r = &mut n; get(r); *r = \
     2; return *r";
  [%expect {| 2 |}];
  (* fails: indexed borrows validate the target path immediately *)
  eval "let xs = [1]; let r = &xs[1]; return 0";
  [%expect {| Runtime error: index 1 out of bounds (length 1) |}];
  (* fails: an indexed borrow cannot move the indexed root in its index expression *)
  typecheck
    "fn take (xs : [int]) -> int { return 0 }; let xs = [7]; let r = &xs[take(xs)]; \
     return 0";
  [%expect {| Borrow error: Cannot move out of xs because it is borrowed. |}];
  (* fails: immutable owners cannot be mutably borrowed *)
  typecheck "let x = 1; let b = &mut x; return 0";
  [%expect {| Type error: Cannot borrow immutable variable x as mutable. |}];
  (* fails: shared references cannot be used for writes *)
  typecheck "let a = 1; let b = &a; *b = 2; return a";
  [%expect {| Type error: Cannot mutate through immutable reference. |}];
  (* fails: references cannot point to references *)
  typecheck "let n = 1; let r = &n; let rr = &r; return 0";
  [%expect {| Type error: References cannot point to references. |}];
  (* fails: function parameters cannot be references to references *)
  typecheck "fn bad (x : & &int) -> int { return 0 }; return 0";
  [%expect {| Type error: References cannot point to references. |}];
  (* fails: references cannot be created through another reference *)
  typecheck "let mut n = 1; let r = &mut n; let shared = &*r; return 0";
  [%expect {| Type error: Cannot borrow through a reference. |}];
  (* fails: reference variables cannot be reassigned *)
  typecheck "let a = 1; let b = 2; let mut r = &a; r = &b; return *r";
  [%expect {| Type error: Reference variables cannot be reassigned. |}];
  (* fails: mutable borrows are exclusive *)
  typecheck "let mut a = 1; let b = &mut a; let c = &a; return 0";
  [%expect
    {| Borrow error: Cannot borrow a as immutable because it is borrowed as mutable. |}];
  (* fails: the persistent borrow graph merges possible borrow edges across branches *)
  typecheck
    "let mut a = 1; let mut b = 2; let r = if True then &mut a else &mut b; a = 3; \
     return 0";
  [%expect {| Borrow error: Cannot assign to a because it is borrowed. |}];
  typecheck
    "let mut a = 1; let mut b = 2; let r = if True then &mut a else &mut b; b = 3; \
     return 0";
  [%expect {| Borrow error: Cannot assign to b because it is borrowed. |}];
  (* ok: a local reference owner is removed from the graph when its block ends *)
  eval "let mut a = 1; { let r = &mut a; *r = 2 }; a = a + 1; return a";
  [%expect {| 3 |}];
  (* fails: moving a mutable reference into a call keeps its referent borrowed *)
  typecheck
    "fn bad (x : &mut int, y : &mut int) -> int { *x = 1; *y = 2; return *x }; let mut n \
     = 0; let r = &mut n; return bad(r, &mut n)";
  [%expect {| Borrow error: Cannot borrow n as mutable because it is already borrowed. |}];
  (* fails: a mutable reference used as shared in one call arg cannot be reused as mutable *)
  typecheck
    "fn bad (x : &int, y : &mut int) -> int { *y = 2; return *x }; let mut n = 1; let r \
     = &mut n; return bad(r, r)";
  [%expect {| Borrow error: Cannot move out of r because it is borrowed. |}];
  (* fails: copyable owners still cannot be read through while mutably borrowed *)
  typecheck "let mut n = 1; let r = &mut n; let copied = n; *r = 2; return copied";
  [%expect {| Borrow error: Cannot copy n because it is borrowed as mutable. |}];
  (* fails: borrowed owners cannot be reassigned *)
  typecheck "let mut a = 1; let b = &a; a = 2; return a";
  [%expect {| Borrow error: Cannot assign to a because it is borrowed. |}];
  (* fails: borrowed owned values cannot be moved *)
  typecheck "let xs = [1]; let b = &xs; let ys = xs; return 0";
  [%expect {| Borrow error: Cannot move out of xs because it is borrowed. |}];
  (* fails: assigning a reference variable to itself is still reassignment *)
  typecheck "let xs = [1]; let mut r = &xs; r = r; let ys = xs; return 0";
  [%expect {| Type error: Reference variables cannot be reassigned. |}];
  (* fails: without lifetimes, functions cannot return references *)
  typecheck "fn bad (x : &int) -> &int { return x }; let a = 1; return *bad(&a)";
  [%expect
    {| Type error: Functions cannot return references without explicit lifetimes. |}]
;;

let%expect_test "threads" =
  (* ok: spawn is a user-facing fork primitive whose result is unit *)
  eval "fn worker (x : int) -> int { return x + 1 }; return spawn(worker(41))";
  [%expect {| () |}];
  (* ok: yield cooperates with Jane Street Async and returns unit *)
  eval "return yield()";
  [%expect {| () |}];
  (* ok: spawned fibers can yield cooperatively without blocking the parent *)
  eval
    "fn spin (mut n : int) -> int { while 0 < n { yield(); n = n - 1 }; return 0 }; \
     spawn(spin(3)); return 1";
  [%expect {| 1 |}];
  (* ok: infinite spawned and parent loops can cooperate through yield *)
  typecheck
    "fn ping () -> unit { while True { yield() }; return yield() }; spawn(ping()); \
     yield(); while True { yield() }; return yield()";
  [%expect {| ok |}];
  (* fails: runtime errors in spawned fibers are propagated to the program *)
  eval "fn bad () -> int { return 1 / 0 }; spawn(bad()); return 42";
  [%expect {| Runtime error: division by zero |}];
  (* ok: owned values can be moved into a spawned thread *)
  eval
    "fn consume (xs : [int]) -> int { return xs[0] }; let xs = [1]; spawn(consume(xs)); \
     return 0";
  [%expect {| 0 |}];
  (* fails: moving a list into a spawned thread consumes it in the parent *)
  typecheck
    "fn consume (xs : [int]) -> int { return xs[0] }; let xs = [1]; spawn(consume(xs)); \
     return xs[0]";
  [%expect {| Borrow error: Value xs used after being moved. |}];
  (* fails: spawned threads cannot borrow parent memory *)
  typecheck
    "fn read (x : &int) -> int { return *x }; let x = 1; spawn(read(&x)); return 0";
  [%expect {| Type error: Spawned threads cannot take reference arguments. |}];
  (* fails: spawn must receive a function call *)
  typecheck "spawn(1); return 0";
  [%expect {| Name error: Builtin spawn expects a function call argument. |}];
  (* fails: yield takes no arguments *)
  typecheck "yield(1); return 0";
  [%expect {| Name error: Builtin yield expects no arguments. |}];
  (* fails: print is not a builtin *)
  typecheck "print(1); return 0";
  [%expect {| Name error: Function print not defined |}]
;;

let%expect_test "list ownership" =
  (* ok: ints are copyable *)
  eval "let x = 1; let y = x; return x + y";
  [%expect {| 2 |}];
  (* ok: indexing a list of copyable values does not move from the list *)
  eval "let xs = [1, 2]; return xs[0] + xs[1]";
  [%expect {| 3 |}];
  (* fails: an index expression cannot move or mutably borrow the indexed list *)
  typecheck "fn take (xs : [int]) -> int { return 0 }; let xs = [7]; return xs[take(xs)]";
  [%expect {| Borrow error: Cannot move out of xs because it is borrowed. |}];
  typecheck
    "fn set0 (xs : &mut [int]) -> int { (*xs)[0] = 9; return 0 }; let mut xs = [1]; \
     return xs[set0(&mut xs)]";
  [%expect
    {| Borrow error: Cannot borrow xs as mutable because it is already borrowed. |}];
  (* fails: list binding transfer moves the source *)
  typecheck "let xs = [1, 2]; let ys = xs; return xs";
  [%expect {| Borrow error: Value xs used after being moved. |}];
  (* fails: indexing cannot move an owned element out of a list *)
  typecheck "let xs = [[1], [2]]; let ys = xs[0]; return 0";
  [%expect {| Borrow error: Cannot move an owned value out of a list by indexing. |}];
  typecheck
    "fn f (xs : [int]) -> int { return xs[0] }; let xs = [[1], [2]]; f(xs[1]); return 0";
  [%expect {| Borrow error: Cannot move an owned value out of a list by indexing. |}];
  (* ok: a bare variable expression statement reads without moving *)
  typecheck "let xs = [1]; xs; xs; return 0";
  [%expect {| ok |}];
  (* ok: assigning a fresh value revives a moved mutable binding *)
  eval "let mut xs = [1]; let ys = xs; xs = [2]; return xs";
  [%expect {| [2] |}];
  (* ok: assigning a function result back to the same binding transfers ownership back *)
  eval
    "fn id (xs : [int]) -> [int] { return xs }; let mut xs = [1]; xs = id(xs); return xs";
  [%expect {| [1] |}];
  (* ok: equality reads owned lists without moving them *)
  eval "let xs = [1, 2]; let ys = [1, 2]; let same = xs == ys; return xs[0] + ys[1]";
  [%expect {| 3 |}];
  (* fails: equality keeps the left owned read borrowed while checking the right side *)
  typecheck "fn id (xs : [int]) -> [int] { return xs }; let xs = [1]; return xs == id(xs)";
  [%expect {| Borrow error: Cannot move out of xs because it is borrowed. |}];
  (* fails: equality cannot read a list while it is mutably borrowed *)
  typecheck "let mut xs = [1]; let r = &mut xs; let same = xs == xs; return 0";
  [%expect {| Borrow error: Cannot read xs because it is borrowed as mutable. |}];
  (* fails: function arguments consume list values *)
  typecheck
    "fn consume (xs : [int]) -> int { return 0 }; let xs = [1]; consume(xs); consume(xs)";
  [%expect {| Borrow error: Value xs used after being moved. |}];
  (* fails: after if-else, a list is live only if both branches leave it live *)
  typecheck "let xs = [1]; if True then { let ys = xs } else { }; return xs";
  [%expect {| Borrow error: Value xs used after being moved. |}];
  (* fails: while bodies may execute more than once, so they cannot leave a live list moved *)
  typecheck
    "fn consume (xs : [int]) -> int { return 0 }; let xs = [1]; while True { consume(xs) \
     }; return 0";
  [%expect {| Borrow error: While body may move list variable xs. |}];
  (* fails: while conditions may also execute more than once *)
  typecheck
    "fn keep_going (xs : [int]) -> bool { return True }; let xs = [1]; while \
     keep_going(xs) { }; return 0";
  [%expect {| Borrow error: While condition may move list variable xs. |}]
;;
