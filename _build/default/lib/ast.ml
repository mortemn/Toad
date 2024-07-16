type node =
  | Statement of statement
  | Expression of expression
  | Program of statement list
and expression =
  | Identifier of ident
  | Int of int 
  | Prefix of { operator : Token.token; right : expression }
  | Infix of { left : expression; operator : Token.token; right : expression }
  | Boolean of { value : bool }
  | If of { condition : expression; consequence : statement list ; alternative : statement list }
  | Function of { parameters : ident list; body : statement list }
  | Call of { call_function : expression; arguments : expression list} 
and statement =
  | Let_Stmt of { name : ident; value : expression }
  | Return_Stmt of expression
  | Expression_Stmt of expression
and ident = { ident : string }
[@@deriving show]