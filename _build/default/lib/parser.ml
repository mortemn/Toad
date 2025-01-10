type parser = {
  lexer : Lexer.lexer;
  current : Token.token option;
  next : Token.token option;
}

type precedence =
  | LOWEST
  | EQUALS
  | LESSGREATER
  | SUM
  | PRODUCT
  | PREFIX
  | CALL
[@@deriving show, ord]

let ( let* ) = Result.bind

let next p =
  let l, n = Lexer.next_token p.lexer in
  { lexer = l ; current = p.next; next = Some n }

let init l =
  let p = { lexer = l; current = None ; next = None } in
  next (next p)

let parse_precedence = function
  | Token.EQ | Token.NOT_EQ -> EQUALS
  | Token.LT | Token.GT -> LESSGREATER
  | Token.PLUS | Token.MINUS -> SUM
  | Token.ASTERISK | Token.SLASH -> PRODUCT
  | Token.LPAREN -> CALL
  | _ -> LOWEST

let rec parse p =
  let rec parse' p statements =
    match p.current with
    | Some Token.EOF -> Ok (p, List.rev statements)
    | Some _ -> (match parse_statement p with
      | Ok (p, stmt) -> parse' (next p) (stmt :: statements)
      | Error msg -> Error (p, msg, statements))
    | None -> Error (p, "No tokens given", statements)
  in
  let* p, statements = parse' p [] in
  Ok (p, Ast.Program statements)

and parse_expression p precedence =
  let* p, prefix = match p.current with
  | Some token -> parse_prefix p token
  | None -> Error "No tokens given"
  in
  let rec recurse_infix p left =
    if peek_semicolon p then Ok (p, left) else
    match peek_precedence p with
    | Ok next_precedence when precedence < next_precedence ->
      (match parse_infix p p.next left with
      | Ok (p, infix) -> recurse_infix p infix
      | Error "No infix parser found" -> Ok (p, left)
      | Error msg -> Error msg)
    | _ -> Ok (p, left)
  in
  recurse_infix p prefix

and parse_statement p =
  match p.current with
  | Some Token.LET -> parse_let p
  | Some Token.RETURN -> parse_return p
  | Some _ -> parse_expression_stmt p
  | None -> Error "No tokens given"

and goto_semicolon p =
  match p.current with
  | Some Token.SEMICOLON -> p
  | _ -> goto_semicolon (next p)

and expect_peek p t =
  match p.next with
  | Some t' when t = t' -> Ok (next p)
  | Some t' -> Error ("Expected " ^ (show t) ^ ", got " ^ (show t'))
  | None -> Error "No tokens given"

and peek_precedence p =
  match p.next with
  | Some t -> Ok (parse_precedence t)
  | None -> Error "No tokens given"

and current_precedence p =
  match p.current with
  | Some t -> Ok (parse_precedence t)
  | None -> Error "No tokens given"

and show t =
  Token.show_token t

and peek_semicolon p =
  match p.next with
  | Some Token.SEMICOLON -> true
  | _ -> false

and parse_ident p =
  let open Ast in
  match p.next with
  | Some (Token.IDENT s) -> Ok (next p, { ident = s })
  | Some t -> Error ("Expected identifier, got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_ident_rec p =
  let open Ast in
  match p.current with
  | Some Token.IDENT ident -> Ok { ident }
  | Some t -> Error ("Expected identifier, got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_ident_expr p =
  let open Ast in
  match p.current with
  | Some Token.IDENT ident -> Ok (p, Identifier { ident })
  | Some t -> Error ("Expected identifier, got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_int_expr p =
  match p.current with
  | Some Token.INT n -> 
    (try Ok (p, Ast.Int (int_of_string n)) with Failure _ -> Error ("Failed to parse int: " ^ n))
  | Some t -> Error ("Expected int, got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_prefix_expr p =
  match p.current with
    | Some t ->
      let p = next p in
      let* p, right = parse_expression p PREFIX in
      Ok (p, Ast.Prefix { operator = t; right})
    | None -> Error "No tokens given"

and parse_infix_expr p left =
  let* operator = match p.current with
    | Some t -> Ok t
    | None -> Error "No tokens given"
  in
  let* precedence = current_precedence p in
  let p = next p in
  let* p, right = parse_expression p precedence in
  Ok (p, Ast.Infix { left; operator; right })

and parse_boolean_expr p =
  match p.current with
  | Some Token.TRUE -> Ok (p, Ast.Boolean { value = true })
  | Some Token.FALSE -> Ok (p, Ast.Boolean { value = false })
  | Some t -> Error ("Expected boolean, got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_grouped_expr p =
  let p = next p in
  let* p, expression = parse_expression p LOWEST in
  match p.next with
  | Some Token.RPAREN -> Ok (next p, expression)
  | Some t -> Error ("Expected ), got " ^ (Token.show_token t))
  | None -> Error "No tokens given"

and parse_if_expr p =
  let* p = expect_peek p Token.LPAREN in
  let p = next p in
  let* p, condition = parse_expression p LOWEST in
  let* p = expect_peek p Token.RPAREN in
  let* p = expect_peek p Token.LBRACE in
  let* p, consequence = parse_block_stmt p in
  match p.next with
  | Some Token.ELSE ->
    let p = next p in
    let* p = expect_peek p Token.LBRACE in
    let* p, alternative = parse_block_stmt p in
    Ok (p, Ast.If { condition; consequence; alternative })
  | _ -> Ok (p, Ast.If { condition; consequence; alternative = [] })

and parse_function_expr p =
  let* p = expect_peek p Token.LPAREN in
  let* p, parameters = parse_function_parameters p in
  let* p = expect_peek p Token.LBRACE in
  let* p, body = parse_block_stmt p in
  Ok (p, Ast.Function { parameters; body })

and parse_call_expr p left =
  let* p, arguments = parse_call_arguments p in
  Ok (p, Ast.Call { call_function = left; arguments })

and parse_prefix p t =
  match t with
  | Token.IDENT _ -> parse_ident_expr p
  | Token.INT _ -> parse_int_expr p
  | Token.BANG | Token.MINUS -> parse_prefix_expr p
  | Token.TRUE | Token.FALSE -> parse_boolean_expr p
  | Token.LPAREN -> parse_grouped_expr p
  | Token.IF -> parse_if_expr p
  | Token.FUNCTION -> parse_function_expr p
  | _ -> Error ("No prefix parser found, got " ^ (Token.show_token t))

and parse_infix p t l =
  match t with
  | Some Token.PLUS | Some Token.MINUS | Some Token.ASTERISK 
  | Some Token.SLASH | Some Token.EQ | Some Token.NOT_EQ 
  | Some Token.LT | Some Token.GT -> parse_infix_expr (next p) l
  | Some Token.LPAREN -> parse_call_expr (next p) l
  | _ -> Error "No infix parser found"

and parse_let p =
  let* p, name = parse_ident p in
  let* p = expect_peek p Token.ASSIGN in
  let p = next p in
  let* p, value = parse_expression p LOWEST in
  let p = if peek_semicolon p then next p else p in
  Ok (p, Ast.Let_Stmt { name; value })

and parse_return p =
  let p = next p in
  let* p, value = parse_expression p LOWEST in
  let p = if peek_semicolon p then next p else p in
  Ok (p, Ast.Return_Stmt (value))

and parse_expression_stmt p =
  let* p, expression = parse_expression p LOWEST in
  let p = if peek_semicolon p then next p else p in
  Ok (p, Ast.Expression_Stmt expression)

and parse_block_stmt p =
  let rec parse_block_stmt' p statements =
    match p.current with
    | Some Token.RBRACE | Some Token.EOF -> 
      Ok (p, (List.rev statements))
    | Some _ -> (match parse_statement p with
      | Ok (p, stmt) -> parse_block_stmt' (next p) (stmt :: statements)
      | Error msg -> Error msg)
    | None -> Error "No tokens given"
  in
  parse_block_stmt' (next p) []

and parse_function_parameters p =
  let rec parse_function_parameters' p parameters =
    match p.next with
    | Some Token.COMMA ->
      let p = next (next p) in
      let* ident = parse_ident_rec p in
      parse_function_parameters' p (ident :: parameters)
    | _ -> Ok (p, List.rev parameters)
  in
  match p.next with
  | Some Token.RPAREN -> Ok (next p, [])
  | Some _ -> 
    let p = next p in
    let* ident = parse_ident_rec p in
    let* p, parameters = parse_function_parameters' p [ident] in
    let* p = expect_peek p Token.RPAREN in
    Ok (p, parameters)
  | None -> Error "No tokens given"

and parse_call_arguments p =
  let rec parse_call_arguments' p parameters =
    match p.next with
    | Some Token.COMMA ->
      let p = next (next p) in
      let* p, arg = parse_expression p LOWEST in
      parse_call_arguments' p (arg :: parameters)
    | _ -> Ok (p, List.rev parameters)
  in
  match p.next with
  | Some Token.RPAREN -> Ok (next p, [])
  | Some _ -> 
    let p = next p in
    let* p, arg = parse_expression p LOWEST in
    let* p, parameters = parse_call_arguments' p [arg] in
    let* p = expect_peek p Token.RPAREN in
    Ok (p, parameters)
  | None -> Error "No tokens given"