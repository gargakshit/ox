open Sexplib0.Sexp_conv

type t = prog
and prog = statement list

and statement =
  | Print of expr (* print expr; *)
  | Expression of expr (* expr; *)
  | VarDeclaration of Token.t * expr (* var Token.Ident = <initializer> expr; *)
  | Block of statement list [@sexp.list] (* { ... } *)
  | If of expr * statement * statement option (* if (cond) ... else ... *)
  | While of expr * statement (* while (cond) ... *)

and expr =
  | Binary of expr * Token.t * expr
  | Unary of Token.t * expr
  | Grouping of expr
  | Literal of Boxed.t
  | Variable of Token.t
  | Assignment of Token.t * expr
[@@deriving show, sexp]
