open Ox.Eval
open Ox.Parser
open Ox.Tokenizer
open Ox.Util
open Sexplib0

let read_file file_name =
  let channel = open_in_bin file_name in
  let contents = really_input_string channel (in_channel_length channel) in
  close_in channel;
  contents

let () =
  let source = read_file "./program.lox" in
  let () = prerr_string "Source: " in
  let () = prerr_endline source in
  let result =
    let* tokens = tokenize source in
    let tokens = List.map (fun (_, tok) -> tok) tokens in
    let* ast = parse tokens in
    prerr_endline "AST:";
    prerr_endline (Sexp.to_string_hum (Ox.Ast.sexp_of_t ast));
    let* result = eval_program ast in
    Ok result
  in
  match result with
  | Error e -> Printf.printf "Execution error: %s\n" e
  | Ok _ -> ()
