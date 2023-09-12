open Sexplib0.Sexp_conv

type t = Num of float | Str of string | Bool of bool | Nil
[@@deriving show, sexp]

let runtime_show = function
  | Num num -> string_of_float num
  | Str str -> str
  | Bool b -> string_of_bool b
  | Nil -> "nil"
