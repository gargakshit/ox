type t = { values : (string, Ast.value) Hashtbl.t; parent : t option }

let init parent = { values = Hashtbl.create 256; parent }
let fork env = init (Some env)

let rec lookup_env env name =
  match Hashtbl.find_opt env.values name with
  | Some _ -> Some env
  | None -> (
      match env.parent with Some env -> lookup_env env name | None -> None)

let rec lookup env name =
  match Hashtbl.find_opt env.values name with
  | Some value -> Some value
  | None -> (
      match env.parent with Some env -> lookup env name | None -> None)

let lookup_res env name =
  match lookup env name with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "Undefined variable '%s'." name)

let add env name value =
  match Hashtbl.find_opt env.values name with
  | Some _ ->
      Error
        (Printf.sprintf
           "Variable '%s' is already bound. Did you mean to assign to '%s'?"
           name name)
  | None -> Ok (Hashtbl.add env.values name value)

let set env name value =
  match lookup_env env name with
  | Some env -> Ok (Hashtbl.replace env.values name value)
  | None -> Error (Printf.sprintf "Undefined variable '%s'." name)
