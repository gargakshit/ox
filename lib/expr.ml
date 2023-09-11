type t =
  | Binary of t * Token.t * t
  | Unary of Token.t * t
  | Grouping of t
  | Literal of Boxed.t
[@@deriving show, sexp]
