open Token

let keyword = function
  | "and" -> Some And
  | "class" -> Some Class
  | "else" -> Some Else
  | "false" -> Some False
  | "fun" -> Some Fun
  | "for" -> Some For
  | "if" -> Some If
  | "nil" -> Some Nil
  | "or" -> Some Or
  | "print" -> Some Print
  | "return" -> Some Return
  | "super" -> Some Super
  | "this" -> Some This
  | "true" -> Some True
  | "var" -> Some Var
  | "while" -> Some While
  | _ -> None

let false_if_none f = function None -> false | Some value -> f value

let is_alpha ch =
  let codepoint = Char.code ch in
  ch = '_'
  || (Char.code 'a' <= codepoint && Char.code 'z' >= codepoint)
  || (Char.code 'A' <= codepoint && Char.code 'Z' >= codepoint)

let is_digit ch =
  let codepoint = Char.code ch in
  Char.code '0' <= codepoint && Char.code '9' >= codepoint

let is_alphanumeric ch = is_alpha ch || is_digit ch

let split_str str start_idx end_idx =
  String.sub str start_idx (end_idx - start_idx)

type tokenizer = { start : int; current : int; line : int; source : string }

let rec is_at_end tokenizer =
  tokenizer.current >= String.length tokenizer.source

and advance tokenizer =
  ( String.get tokenizer.source tokenizer.current,
    { tokenizer with current = tokenizer.current + 1 } )

and peek tokenizer =
  if is_at_end tokenizer then None
  else Some (String.get tokenizer.source tokenizer.current)

and peek_next tokenizer =
  if tokenizer.current + 1 >= String.length tokenizer.source then None
  else Some (String.get tokenizer.source (tokenizer.current + 1))

and match' tokenizer char =
  if is_at_end tokenizer then (false, tokenizer)
  else
    let ch = String.get tokenizer.source tokenizer.current in
    if ch = char then (true, { tokenizer with current = tokenizer.current + 1 })
    else (false, tokenizer)

and chomp_till_end_of_line tokenizer =
  if is_at_end tokenizer then tokenizer
  else
    let ch = String.get tokenizer.source tokenizer.current in
    if ch = '\n' then tokenizer
    else
      chomp_till_end_of_line { tokenizer with current = tokenizer.current + 1 }

and str tokenizer =
  match peek tokenizer with
  | None -> (tokenizer, Error "Unterminated string literal.")
  | Some '"' ->
      ( { tokenizer with current = tokenizer.current + 1 },
        Ok (split_str tokenizer.source (tokenizer.start + 1) tokenizer.current)
      )
  | Some '\n' ->
      str
        {
          tokenizer with
          current = tokenizer.current + 1;
          line = tokenizer.line + 1;
        }
  | Some _ -> str { tokenizer with current = tokenizer.current + 1 }

and ident tokenizer =
  match peek tokenizer with
  | Some ch when is_alphanumeric ch ->
      ident { tokenizer with current = tokenizer.current + 1 }
  | _ ->
      let ident' =
        split_str tokenizer.source tokenizer.start tokenizer.current
      in
      let token =
        match keyword ident' with None -> Ident ident' | Some token -> token
      in
      (tokenizer, token)

and num tokenizer =
  let rec num' tokenizer dot_encountered =
    match peek tokenizer with
    | Some ch when is_digit ch ->
        num' { tokenizer with current = tokenizer.current + 1 } dot_encountered
    | Some '.'
      when dot_encountered = false
           && false_if_none is_digit (peek_next tokenizer) ->
        num' { tokenizer with current = tokenizer.current + 1 } true
    | _ ->
        ( tokenizer,
          Num
            (Float.of_string
               (split_str tokenizer.source tokenizer.start tokenizer.current))
        )
  in
  num' tokenizer false

and scan tokenizer =
  let char, tokenizer = advance tokenizer in
  let tokenizer, token =
    match char with
    (* Single character tokens *)
    | '(' -> (tokenizer, Ok (Some LParen))
    | ')' -> (tokenizer, Ok (Some RParen))
    | '{' -> (tokenizer, Ok (Some LBrace))
    | '}' -> (tokenizer, Ok (Some RBrace))
    | ',' -> (tokenizer, Ok (Some Comma))
    | '.' -> (tokenizer, Ok (Some Dot))
    | '-' -> (tokenizer, Ok (Some Minus))
    | '+' -> (tokenizer, Ok (Some Plus))
    | ';' -> (tokenizer, Ok (Some SemiColon))
    | '*' -> (tokenizer, Ok (Some Star))
    (* One or two character tokens *)
    | '!' ->
        let matched, tokenizer = match' tokenizer '=' in
        (tokenizer, Ok (Some (if matched then BangEqual else Bang)))
    | '=' ->
        let matched, tokenizer = match' tokenizer '=' in
        (tokenizer, Ok (Some (if matched then EqualEqual else Equal)))
    | '<' ->
        let matched, tokenizer = match' tokenizer '=' in
        (tokenizer, Ok (Some (if matched then LessEqual else Less)))
    | '>' ->
        let matched, tokenizer = match' tokenizer '=' in
        (tokenizer, Ok (Some (if matched then GreaterEqual else Greater)))
    | '/' ->
        let matched, tokenizer = match' tokenizer '/' in
        if matched then
          (* We have a comment. Chomp till the end of the line. *)
          (chomp_till_end_of_line tokenizer, Ok None)
        else (tokenizer, Ok (Some Slash))
    | '"' ->
        let tokenizer, res = str tokenizer in
        let ret =
          match res with
          | Ok str -> Ok (Some (Str str))
          | Error err -> Error err
        in
        (tokenizer, ret)
    (* Whitespaces *)
    | '\r' | '\t' | ' ' -> (tokenizer, Ok None)
    | '\n' -> ({ tokenizer with line = tokenizer.line + 1 }, Ok None)
    | ch when is_alpha ch ->
        let tokenizer, token = ident tokenizer in
        (tokenizer, Ok (Some token))
    | ch when is_digit ch ->
        let tokenizer, token = num tokenizer in
        (tokenizer, Ok (Some token))
    | _ ->
        ( tokenizer,
          Error
            (Printf.sprintf "Line %d: Unexpected character '%c'." tokenizer.line
               char) )
  in
  (token, tokenizer)

let rec scan_all tokenizer acc =
  if is_at_end tokenizer then Ok (List.rev ((tokenizer.line, Eof) :: acc))
  else
    let token, tokenizer' = scan tokenizer in
    match token with
    | Error e -> Error e
    | Ok token ->
        let acc =
          match token with
          | None -> acc
          | Some token -> (tokenizer.line, token) :: acc
        in
        scan_all { tokenizer' with start = tokenizer'.current } acc

let tokenize source =
  let tokenizer = { line = 0; start = 0; current = 0; source } in
  scan_all tokenizer []
