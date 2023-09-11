open Ox.Parser
open Ox.Tokenizer
open Ox.Util
open Sexplib0

let () =
  let source = "10 + 20 / (5 * 40)" in
  let ast =
    let* tokens = tokenize source in
    let tokens = List.map (fun (_, tok) -> tok) tokens in
    let* ast = parse tokens in
    Ok ast
  in
  match ast with
  | Error e -> Printf.printf "Error: %s" e
  | Ok ast ->
      let () = prerr_endline source in
      let () = print_endline (Sexp.to_string_hum (Ox.Expr.sexp_of_t ast)) in
      ()
