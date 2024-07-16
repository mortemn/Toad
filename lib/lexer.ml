type lexer = {
  input : string;
  position : int;
  ch : char
}

let init s =
  { input = s; position = 0; ch = String.get s 0 }

let read_char lexer =
  let np = lexer.position + 1 in (* new position *)
  if np >= String.length lexer.input then
    { lexer with ch = '\x00'; position = np }
  else
    { lexer with ch = String.get lexer.input np; position = np }

let rec skip_whitespace lexer =
  match lexer.ch with
    | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (read_char lexer)
    | _ -> lexer

let peek lexer =
  let np = lexer.position + 1 in
  if np >= String.length lexer.input then
    '\x00'
  else
    String.get lexer.input np

let is_next lexer ch t_token f_token =
  if peek lexer == ch then
    read_char (read_char lexer), t_token
  else
    read_char lexer, f_token

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let rec read_ident lexer start =
  if is_letter lexer.ch then read_ident (read_char lexer) start
  else lexer, (String.sub lexer.input start (lexer.position - start))

let rec read_number lexer start =
  if is_digit lexer.ch then read_number (read_char lexer) start
  else lexer, (String.sub lexer.input start (lexer.position - start))

let return_ident lexer =
  let lexer, ident = read_ident lexer lexer.position in
  lexer, Token.lookup_ident(ident)

let return_int lexer =
  let lexer, n = read_number lexer lexer.position in
  lexer, Token.INT(n)

let next_token lexer =
  let lexer = skip_whitespace lexer in
  match lexer.ch with
  | '=' -> is_next lexer '=' Token.EQ Token.ASSIGN
  | '+' -> read_char lexer, Token.PLUS 
  | '-' -> read_char lexer, Token.MINUS
  | '!' -> is_next lexer '=' Token.NOT_EQ Token.BANG
  | '/' -> read_char lexer, Token.SLASH
  | '*' -> read_char lexer, Token.ASTERISK
  | '<' -> read_char lexer, Token.LT
  | '>' -> read_char lexer, Token.GT
  | ';' -> read_char lexer, Token.SEMICOLON
  | ',' -> read_char lexer, Token.COMMA
  | '{' -> read_char lexer, Token.LBRACE
  | '}' -> read_char lexer, Token.RBRACE
  | '(' -> read_char lexer, Token.LPAREN
  | ')' -> read_char lexer, Token.RPAREN
  | '\x00' -> read_char lexer, Token.EOF
  | ch when is_letter ch -> return_ident lexer
  | ch when is_digit ch -> return_int lexer
  | _ -> read_char lexer, Token.ILLEGAL
