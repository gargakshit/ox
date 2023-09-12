open Ox.Eval
open Ox.Parser
open Ox.Tokenizer
open Ox.Util
open Sexplib0

let () =
  let source =
    "\n\
    \    print (42 * 42 - (42 / 42)) == 1763;\n\
    \    print \"Lol\";\n\
    \    print nil;\n\
    \    print true == true;\n\
    \    var a;\n\
    \    var b = 42;\n\
    \    print 420 - b;\n\
    \    print (42 * 42 - (42 / 41));\n\
    \    b = 41;\n\
    \    print b;\n\
    \    {\n\
    \        print b;\n\
    \        var b = 10;\n\
    \        print b;\n\
    \    }\n\
    \  "
  in
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
  match result with
  | Error e -> Printf.printf "Execution error: %s\n" e
  | Ok _ -> ()
