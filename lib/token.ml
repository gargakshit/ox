open Sexplib0.Sexp_conv

type t =
  (* Single character tokens *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Comma
  | Dot
  | Plus
  | Minus
  | SemiColon
  | Slash
  | Star
  (* One or two character tokens *)
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  (* Operators and Keywords *)
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  (* Literals *)
  | Ident of string
  | Str of string
  | Num of float
  (* Special *)
  | Eof
[@@deriving show, sexp]
