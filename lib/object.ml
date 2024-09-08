type obj =
  | Integer of int
  | Boolean of bool
  | ReturnValue of obj
  | Function of { 
      parameters : Ast.ident list ;
      body : Ast.statement list ;
      env : Environment.environment }
  | Null

let rec to_string = function
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | ReturnValue o -> to_string o
  | Function _ -> "Placeholder"
  | Null -> "null"