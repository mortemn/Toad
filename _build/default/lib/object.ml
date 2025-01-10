type obj =
  | Integer of int
  | Boolean of bool
  | ReturnValue of obj
  | Function of function_obj
  | Null
and function_obj =
  {
    parameters : Ast.ident list ;
    body : Ast.statement list ;
    env : obj Environment.environment
  }
[@@deriving show]

let rec to_string = function
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | ReturnValue o -> to_string o
  | Function _ -> "Function placeholder"
  | Null -> "null"