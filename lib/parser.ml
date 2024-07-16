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

and parse_prefix p t =
  match t with
  | Token.IDENT _ -> parse_ident_expr p
  | Token.INT _ -> parse_int_expr p
  | Token.BANG | Token.MINUS -> parse_prefix_expr p
  | _ -> Error ("No prefix parser found, got " ^ (Token.show_token t))

and parse_infix p t l =
  match t with
  | Some Token.PLUS | Some Token.MINUS | Some Token.ASTERISK 
  | Some Token.SLASH | Some Token.EQ | Some Token.NOT_EQ 
  | Some Token.LT | Some Token.GT -> parse_infix_expr (next p) l
  | _ -> Error "No infix parser found"

and parse_let p =
  let* p, name = parse_ident p in
  let value = Ast.Int(42) in
  let p = goto_semicolon p in
  Ok (p, Ast.Let_Stmt { name; value })

and parse_return p =
  let p = goto_semicolon p in
  Ok (p, Ast.Return_Stmt (Ast.Int 42))

and parse_expression_stmt p =
  let* p, expression = parse_expression p LOWEST in
  let p = if peek_semicolon p then next p else p in
  Ok (p, Ast.Expression_Stmt expression)