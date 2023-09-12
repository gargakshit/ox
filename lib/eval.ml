open Ast
open Eff
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

let is_truthy = function
  | Boxed.Bool true -> true
  | Boxed.Bool false | Nil -> false
  | _ -> true

let is_equal left right =
  match (left, right) with
  | Boxed.Nil, Boxed.Nil -> true
  | Boxed.Bool left, Boxed.Bool right -> left = right
  | Boxed.Num left, Boxed.Num right -> left = right
  | Boxed.Str left, Boxed.Str right -> left = right
  | _ -> false

let require_num = function
  | Boxed.Num num -> Ok num
  | _ -> Error "Expected number."

let require_str = function
  | Boxed.Str str -> Ok str
  | _ -> Error "Expected string."

let is_num = function Boxed.Num _ -> true | _ -> false
let eff_print value = Effect.perform (Eff.Ox_print value)

let rec eval_stmt = function
  | Print expr ->
      let* value = eval_expr expr in
      Ok (eff_print value)
  | Expression expr ->
      let* value = eval_expr expr in
      Ok ()

and eval_expr = function
  | Literal lit -> Ok lit
  | Grouping expr -> eval_expr expr
  | Unary (Token.Bang, expr) -> unary_bang expr
  | Unary (Token.Minus, expr) -> unary_minus expr
  | Binary (left, operator, right) when is_arith operator ->
      binary left operator right
  | Binary (left, operator, right) when is_comparison operator ->
      binary_comparison left operator right
  | Binary (left, operator, right) when is_equality operator ->
      binary_equality left operator right
  | _ -> failwith "Unreachable."

and unary_bang expr =
  let* expr = eval_expr expr in
  Ok (Boxed.Bool (is_truthy expr))

and unary_minus expr =
  let* expr = eval_expr expr in
  let* expr = require_num expr in
  Ok (Boxed.Num (-.expr))

and binary left operator right =
  let* left = eval_expr left in
  let* right = eval_expr right in
  match operator with
  | Token.Minus | Token.Slash | Token.Star -> binary_arith left operator right
  | Token.Plus -> binary_plus left right
  | _ -> failwith "Unreachable."

and binary_plus left right =
  if is_num left then binary_arith left Token.Plus right
  else
    let* left = require_str left in
    let* right = require_str right in
    Ok (Boxed.Str (String.cat left right))

and binary_arith left operator right =
  let* left = require_num left in
  let* right = require_num right in
  match operator with
  | Token.Minus -> Ok (Boxed.Num (left -. right))
  | Token.Plus -> Ok (Boxed.Num (left +. right))
  | Token.Slash -> Ok (Boxed.Num (left /. right))
  | Token.Star -> Ok (Boxed.Num (left *. right))
  | _ -> failwith "Unreachable."

and binary_comparison left operator right =
  let* left = eval_expr left in
  let* left = require_num left in
  let* right = eval_expr right in
  let* right = require_num right in
  match operator with
  | Token.Greater -> Ok (Boxed.Bool (left > right))
  | Token.GreaterEqual -> Ok (Boxed.Bool (left >= right))
  | Token.Less -> Ok (Boxed.Bool (left < right))
  | Token.LessEqual -> Ok (Boxed.Bool (left <= right))
  | _ -> failwith "Unreachable."

and binary_equality left operator right =
  let* left = eval_expr left in
  let* right = eval_expr right in
  let eq = is_equal left right in
  match operator with
  | Token.BangEqual -> Ok (Boxed.Bool (Bool.not eq))
  | Token.EqualEqual -> Ok (Boxed.Bool eq)
  | _ -> failwith "Unreachable."

let eval_program ast = Effect.Deep.match_with eval_stmt ast Eff.Default.handler
