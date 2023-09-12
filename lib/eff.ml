open Effect
open Effect.Deep

type _ Effect.t += Ox_print : Ast.value -> unit Effect.t

module Default = struct
  let ox_print value = print_endline (Ast.runtime_show_value value)

  let handler =
    {
      effc =
        (fun (type c) (eff : c Effect.t) ->
          match eff with
          | Ox_print value ->
              Some
                (fun (k : (c, _) continuation) -> continue k (ox_print value))
          | _ -> None);
      exnc = (fun exn -> raise exn);
      retc = (fun v -> v);
    }
end
