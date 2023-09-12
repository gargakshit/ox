open Util

type parser = { remaining : Token.t list }

let expect_assignment_target = function
  | Ast.Variable name -> Ok name
  | _ -> Error "Invalid assignment target."

let rec expression parser = assignment parser

and assignment parser =
  let* parser, target = or' parser in
  match peek parser with
  | Token.Equal ->
      let parser = advance parser in
      let* parser, value = assignment parser in
      let* name = expect_assignment_target target in
      Ok (parser, Ast.Assignment (name, value))
  | _ -> Ok (parser, target)

and or' parser =
  let rec or'' parser expr =
    let operator = peek parser in
    match operator with
    | Token.Or ->
        let parser = advance parser in
        let* parser, right = and' parser in
        or'' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = and' parser in
  or'' parser expr

and and' parser =
  let rec and'' parser expr =
    let operator = peek parser in
    match operator with
    | Token.And ->
        let parser = advance parser in
        let* parser, right = equality parser in
        and'' parser (Ast.Binary (expr, operator, right))
    | _ -> Ok (parser, expr)
  in
  let* parser, expr = equality parser in
  and'' parser expr

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
  | Token.Ident _ -> Ok (parser, Ast.Variable token)
  | token -> Error (Printf.sprintf "Unexpected token '%s'." (Token.show token))

and consume parser token error =
  let token' = peek parser in
  if token' = token then Ok (advance parser) else Error error

and advance parser = { remaining = List.tl parser.remaining }
and peek parser = List.hd parser.remaining

and is_at_end parser =
  match parser.remaining with [] | [ Token.Eof ] -> true | _ -> false

and parse_prog parser acc =
  if is_at_end parser then Ok (List.rev acc)
  else
    let* parser, stmt = stmt parser in
    parse_prog parser (stmt :: acc)

and parse_stmts parser acc =
  if is_at_end parser then Error "Expected '}'."
  else
    match peek parser with
    | Token.RBrace -> Ok (parser, List.rev acc)
    | _ ->
        let* parser, stmt = stmt parser in
        parse_stmts parser (stmt :: acc)

and stmt parser = declaration parser

and declaration parser =
  match peek parser with
  | Token.Var -> var_declaration (advance parser)
  | _ -> statement parser

and var_declaration parser =
  let ident = peek parser in
  match ident with
  | Token.Ident _ ->
      let parser = advance parser in
      let* parser, expr = equal_initializer parser in
      let* parser = expect_semicolon parser in
      Ok (parser, Ast.VarDeclaration (ident, expr))
  | _ -> Error "Expected identifier after var."

and statement parser =
  match peek parser with
  | Token.Print -> print_statement (advance parser)
  | Token.LBrace -> block_stmt (advance parser)
  | Token.If -> if_stmt (advance parser)
  | Token.While -> while_stmt (advance parser)
  | Token.For -> for_stmt (advance parser)
  | _ -> expression_statement parser

and for_stmt parser =
  let* parser = consume parser Token.LParen "Expected '(' after for." in
  let* parser, initializer_stmt =
    match peek parser with
    | Token.SemiColon -> Ok (advance parser, None)
    | Token.Var ->
        let* parser, stmt = var_declaration (advance parser) in
        Ok (parser, Some stmt)
    | _ ->
        let* parser, stmt = expression_statement parser in
        Ok (parser, Some stmt)
  in
  let* parser, condition_expr =
    match peek parser with
    | Token.SemiColon -> Ok (advance parser, None)
    | _ ->
        let* parser, expr = expression parser in
        let* parser =
          consume parser Token.SemiColon "Expected ';' after the condition."
        in
        Ok (parser, Some expr)
  in
  let* parser, increment_expr =
    match peek parser with
    | Token.RParen -> Ok (advance parser, None)
    | _ ->
        let* parser, expr = expression parser in
        let* parser =
          consume parser Token.RParen "Expected ')' after 'for' clauses."
        in
        Ok (parser, Some expr)
  in
  let* parser, body = statement parser in
  let body =
    match increment_expr with
    | None -> body
    | Some increment_expr -> Ast.Block [ body; Ast.Expression increment_expr ]
  in
  let condition_expr =
    match condition_expr with
    | None -> Ast.Literal (Boxed.Bool true)
    | Some expr -> expr
  in
  let body = Ast.While (condition_expr, body) in
  Ok
    ( parser,
      match initializer_stmt with
      | None -> body
      | Some initializer_stmt -> Ast.Block [ initializer_stmt; body ] )

and while_stmt parser =
  let* parser = consume parser Token.LParen "Expected '(' after while." in
  let* parser, guard = expression parser in
  let* parser =
    consume parser Token.RParen "Expected ')' after the condition."
  in
  let* parser, body = statement parser in
  Ok (parser, Ast.While (guard, body))

and if_stmt parser =
  let* parser = consume parser Token.LParen "Expected '(' after if." in
  let* parser, guard = expression parser in
  let* parser =
    consume parser Token.RParen "Expected ')' after the condition."
  in
  let* parser, then_stmt = statement parser in
  let token = peek parser in
  let* parser, else_stmt =
    if token = Token.Else then
      let parser = advance parser in
      let* parser, else_stmt = statement parser in
      Ok (parser, Some else_stmt)
    else Ok (parser, None)
  in
  Ok (parser, Ast.If (guard, then_stmt, else_stmt))

and block_stmt parser =
  let* parser, stmts = parse_stmts parser [] in
  let parser = advance parser in
  Ok (parser, Ast.Block stmts)

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

and equal_initializer parser =
  match peek parser with
  | Token.Equal ->
      let parser = advance parser in
      let* parser, expr = expression parser in
      Ok (parser, expr)
  | _ -> Ok (parser, Literal Boxed.Nil)

let parse tokens =
  let* prog = parse_prog { remaining = tokens } [] in
  Ok prog
