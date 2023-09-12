open Util

type parser = { remaining : Token.t list }

let rec expression parser = equality parser

and equality parser =
  let rec equality' parser expr =
    let operator = peek parser in
    match operator with
    | Token.Bang | Token.BangEqual ->
        let parser = advance parser in
        let* parser, right = comparison parser in
        equality' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = comparison parser in
  equality' parser expr

and comparison parser =
  let rec comparison' parser expr =
    let operator = peek parser in
    match operator with
    | Token.BangEqual | Token.EqualEqual | Token.Greater | Token.GreaterEqual
    | Token.Less | Token.LessEqual ->
        let parser = advance parser in
        let* parser, right = term parser in
        comparison' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = term parser in
  comparison' parser expr

and term parser =
  let rec term' parser expr =
    let operator = peek parser in
    match operator with
    | Token.Minus | Token.Plus ->
        let parser = advance parser in
        let* parser, right = factor parser in
        term' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = factor parser in
  term' parser expr

and factor parser =
  let rec factor' parser expr =
    let operator = peek parser in
    match operator with
    | Token.Slash | Token.Star ->
        let parser = advance parser in
        let* parser, right = unary parser in
        factor' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = unary parser in
  factor' parser expr

and unary parser =
  let operator = peek parser in
  match operator with
  | Token.Bang | Token.Minus ->
      let parser = advance parser in
      let* parser, expr = unary parser in
      Ok (parser, Ast.Unary (operator, expr))
  | _ -> primary parser

and primary parser =
  let token = peek parser in
  let parser = advance parser in
  match token with
  | Token.False -> Ok (parser, Ast.Literal (Boxed.Bool false))
  | Token.True -> Ok (parser, Ast.Literal (Boxed.Bool true))
  | Token.Nil -> Ok (parser, Ast.Literal Boxed.Nil)
  | Token.Num num -> Ok (parser, Ast.Literal (Boxed.Num num))
  | Token.Str str -> Ok (parser, Ast.Literal (Boxed.Str str))
  | Token.LParen ->
      let* parser, expr = expression parser in
      let* parser =
        consume parser Token.RParen "Expected ')' after expression."
      in
      Ok (parser, Ast.Grouping expr)
  | token -> Error (Printf.sprintf "Unexpected token '%s'." (Token.show token))

and consume parser token error =
  let token' = peek parser in
  if token' = token then Ok (advance parser) else Error error

and advance parser = { remaining = List.tl parser.remaining }
and peek parser = List.hd parser.remaining

let rec statement parser =
  match peek parser with
  | Token.Print ->
      let parser = advance parser in
      print_statement parser
  | _ -> expression_statement parser

and print_statement parser =
  let* parser, expr = expression parser in
  let* parser = expect_semicolon parser in
  Ok (parser, Ast.Print expr)

and expression_statement parser =
  let* parser, expr = expression parser in
  let* parser = expect_semicolon parser in
  Ok (parser, Ast.Expression expr)

and expect_semicolon parser =
  match peek parser with
  | Token.SemiColon -> Ok (advance parser)
  | _ -> Error "Expected ';'."

let parse tokens =
  let* parser, expr = statement { remaining = tokens } in
  Ok expr
