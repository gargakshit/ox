open Ox.Eval
open Ox.Parser
open Ox.Tokenizer
open Ox.Util
open Sexplib0

let () =
  let source = "print (42 * 42 - (42 / 42)) == 1763;" in
  let () = prerr_string "Source: " in
  let () = prerr_endline source in
  let result =
    let* tokens = tokenize source in
    let tokens = List.map (fun (_, tok) -> tok) tokens in
    let* ast = parse tokens in
    let () = prerr_endline "AST:" in
    let () = prerr_endline (Sexp.to_string_hum (Ox.Ast.sexp_of_t ast)) in
    let* result = eval_program ast in
    Ok result
  in
  match result with Error e -> Printf.printf "Error: %s\n" e | Ok _ -> ()
