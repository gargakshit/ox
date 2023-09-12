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
  | Literal of value
  | Variable of Token.t
  | Assignment of Token.t * expr
  | Call of expr * Token.t * expr list * int (* callee(argument, list, ...) *)

and value = Num of float | Str of string | Bool of bool | Nil
[@@deriving show, sexp]

let runtime_show_value = function
  | Num num -> string_of_float num
  | Str str -> str
  | Bool b -> string_of_bool b
  | Nil -> "nil"
