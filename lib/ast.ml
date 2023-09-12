type t = statement
and statement = Print of expr | Expression of expr

and expr =
  | Binary of expr * Token.t * expr
  | Unary of Token.t * expr
  | Grouping of expr
  | Literal of Boxed.t
[@@deriving show, sexp]
