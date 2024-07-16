type token =
  | ILLEGAL
  | EOF

  | IDENT of string
  | INT of string

  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH

  | LT
  | GT

  | COMMA
  | SEMICOLON

  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE

  | FUNCTION
  | LET
  | IF
  | ELSE
  | RETURN
  | TRUE
  | FALSE
  | EQ
  | NOT_EQ
[@@deriving show]

let lookup_ident = function
  | "function" -> FUNCTION
  | "let" -> LET
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "return" -> RETURN
  | ident -> IDENT(ident)
