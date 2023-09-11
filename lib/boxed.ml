open Sexplib0.Sexp_conv

type t = Num of float | Str of string | Bool of bool | Nil
[@@deriving show, sexp]
