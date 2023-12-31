open Ast
open Eff
open Effect
open Effect.Deep
open Env
open Util

let is_arith = function
  | Token.Minus | Token.Plus | Token.Slash | Token.Star -> true
  | _ -> false

let is_equality = function
  | Token.BangEqual | Token.EqualEqual -> true
  | _ -> false

let is_comparison = function
  | Token.Greater | Token.GreaterEqual | Token.Less | Token.LessEqual -> true
  | _ -> false

let is_logic = function Token.And | Token.Or -> true | _ -> false

let is_truthy = function
  | Ast.Bool true -> true
  | Ast.Bool false | Nil -> false
  | _ -> true

let is_equal left right =
  match (left, right) with
  | Ast.Nil, Ast.Nil -> true
  | Ast.Bool left, Ast.Bool right -> left = right
  | Ast.Num left, Ast.Num right -> left = right
  | Ast.Str left, Ast.Str right -> left = right
  | _ -> false

let require_num = function
  | Ast.Num num -> Ok num
  | _ -> Error "Expected number."

let require_str = function
  | Ast.Str str -> Ok str
  | _ -> Error "Expected string."

let require_ident = function
  | Token.Ident name -> Ok name
  | _ -> Error "Expected identifier."

let is_num = function Ast.Num _ -> true | _ -> false
let eff_print value = perform (Eff.Ox_print value)

let rec eval_stmts env = function
  | [] -> Ok ()
  | stmt :: rest ->
      let* _ = eval_stmt env stmt in
      eval_stmts env rest

and eval_stmt env = function
  | Print expr ->
      let* value = eval_expr env expr in
      Ok (eff_print value)
  | Expression expr ->
      let* value = eval_expr env expr in
      Ok ()
  | VarDeclaration (ident, expr) -> eval_var_decl env ident expr
  | Block stmts -> eval_block env stmts
  | If (guard, then_stmt, else_stmt) ->
      eval_if_stmt env guard then_stmt else_stmt
  | While (cond, body) -> eval_while_stmt env cond body

and eval_while_stmt env cond body =
  let* cond' = eval_expr env cond in
  if is_truthy cond' then
    let* _ = eval_stmt env body in
    eval_while_stmt env cond body
  else Ok ()

and eval_if_stmt env guard then_stmt else_stmt =
  let* guard = eval_expr env guard in
  if is_truthy guard then eval_stmt env then_stmt
  else
    match else_stmt with
    | Some else_stmt -> eval_stmt env else_stmt
    | None -> Ok ()

and eval_block env = eval_stmts (Env.fork env)

and eval_var_decl env ident expr =
  let* name = require_ident ident in
  let* value = eval_expr env expr in
  Env.add env name value

and eval_expr env = function
  | Literal lit -> Ok lit
  | Grouping expr -> eval_expr env expr
  | Unary (Token.Bang, expr) -> unary_bang env expr
  | Unary (Token.Minus, expr) -> unary_minus env expr
  | Binary (left, operator, right) when is_arith operator ->
      binary env left operator right
  | Binary (left, operator, right) when is_comparison operator ->
      binary_comparison env left operator right
  | Binary (left, operator, right) when is_equality operator ->
      binary_equality env left operator right
  | Binary (left, operator, right) when is_logic operator ->
      binary_logic env left operator right
  | Variable ident -> variable env ident
  | Assignment (ident, expr) -> assignment env ident expr
  | Call (callee, _, arguments, num_args) -> failwith "Unimplemented."
  | Binary _ | Unary _ -> failwith "Unreachable."

and binary_logic env left operator right =
  let* left = eval_expr env left in
  let left = is_truthy left in
  match operator with
  | Token.And when not left -> Ok (Ast.Bool true)
  | Token.Or when left -> Ok (Ast.Bool true)
  | _ -> eval_expr env right

and assignment env ident expr =
  let* name = require_ident ident in
  let* value = eval_expr env expr in
  let* () = Env.set env name value in
  Ok value

and variable env = function
  | Token.Ident name -> Env.lookup_res env name
  | _ -> failwith "Unreachable."

and unary_bang env expr =
  let* expr = eval_expr env expr in
  Ok (Ast.Bool (is_truthy expr))

and unary_minus env expr =
  let* expr = eval_expr env expr in
  let* expr = require_num expr in
  Ok (Ast.Num (-.expr))

and binary env left operator right =
  let* left = eval_expr env left in
  let* right = eval_expr env right in
  match operator with
  | Token.Minus | Token.Slash | Token.Star -> binary_arith left operator right
  | Token.Plus -> binary_plus left right
  | _ -> failwith "Unreachable."

and binary_plus left right =
  if is_num left then binary_arith left Token.Plus right
  else
    let* left = require_str left in
    let* right = require_str right in
    Ok (Ast.Str (String.cat left right))

and binary_arith left operator right =
  let* left = require_num left in
  let* right = require_num right in
  match operator with
  | Token.Minus -> Ok (Ast.Num (left -. right))
  | Token.Plus -> Ok (Ast.Num (left +. right))
  | Token.Slash -> Ok (Ast.Num (left /. right))
  | Token.Star -> Ok (Ast.Num (left *. right))
  | _ -> failwith "Unreachable."

and binary_comparison env left operator right =
  let* left = eval_expr env left in
  let* left = require_num left in
  let* right = eval_expr env right in
  let* right = require_num right in
  match operator with
  | Token.Greater -> Ok (Ast.Bool (left > right))
  | Token.GreaterEqual -> Ok (Ast.Bool (left >= right))
  | Token.Less -> Ok (Ast.Bool (left < right))
  | Token.LessEqual -> Ok (Ast.Bool (left <= right))
  | _ -> failwith "Unreachable."

and binary_equality env left operator right =
  let* left = eval_expr env left in
  let* right = eval_expr env right in
  let eq = is_equal left right in
  match operator with
  | Token.BangEqual -> Ok (Ast.Bool (Bool.not eq))
  | Token.EqualEqual -> Ok (Ast.Bool eq)
  | _ -> failwith "Unreachable."

let initial_env () = Env.init None

let eval_program prog =
  let eval () = eval_stmts (initial_env ()) prog in
  match_with eval () Eff.Default.handler
