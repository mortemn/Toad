type t =
  | Node of Ast.node
  | Expression of Ast.expression
  | Statement of Ast.statement

let ( let* ) = Result.bind

let rec eval obj env =
  match obj with
  | Node n -> eval_node n env
  | Expression e -> eval_expression e env
  | Statement s -> eval_statement s env

and eval_node obj env =
  match obj with
  | Ast.Program statements -> eval_program (wrap_statements statements) Object.Null env

and eval_expression obj env =
  match obj with
  | Ast.Int integer -> Ok (Object.Integer integer)
  | Ast.Boolean { value } -> Ok (Object.Boolean value)
  | Ast.Prefix { operator; right } -> eval_prefix_expr operator (eval (Expression right) env)
  | Ast.Infix { left; operator; right } -> 
    eval_infix_expr operator (eval (Expression left) env) (eval (Expression right) env)
  | Ast.If { condition; consequence; alternative } ->  eval_if_expr condition consequence alternative env
  | Ast.Identifier { ident } -> Environment.get env ident
  | Ast.Function { parameters; body } ->  Ok (Object.Function { parameters; body; env })
  | Ast.Call { call_function; arguments } -> (
    let* function_obj = eval (Expression call_function) env in
    let* arguments = eval_expressions arguments env in
    apply_function function_obj arguments
  )

and eval_statement obj env =
  match obj with
  | Ast.Expression_Stmt expression -> eval_expression expression env
  | Ast.Return_Stmt expression ->
    (match eval (Expression expression) env with
    | Ok obj -> Ok (Object.ReturnValue obj)
    | Error msg -> Error msg)
  | Ast.Let_Stmt { name; value } ->
    let* value = eval (Expression value) env in
    Environment.set env name.ident value;
    Ok (Object.Null)

and eval_expressions expressions env =
  match expressions with
  | [] -> Ok []
  | h :: t -> (match eval (Expression h) env with
    | Ok obj -> let* rest = eval_expressions t env in Ok (obj :: rest)
    | Error msg -> Error msg)

and apply_function function_obj arguments =
  match function_obj with
  | Object.Function { parameters; body; env } ->
    let* extended_env = extend_env env parameters arguments in
    let* evaluated = eval_block body Object.Null extended_env in
    Ok(evaluated)
  | _ -> Error "Not a function"

and extend_env env parameters arguments =
  let env = Environment.new_enclosed env in
  let rec extend_env' env parameters arguments =
    match parameters, arguments with
    | [], [] -> Ok env
    | h1 :: t1, h2 :: t2 -> (
      Environment.set env h1.Ast.ident h2;
      extend_env' env t1 t2
    )
    | _ -> Error "Invalid function call"
  in
  extend_env' env parameters arguments

and eval_program statements evaluation env =
  match statements with
  | [] ->  Ok (evaluation)
  | h :: t -> match eval h env with
    | Ok Object.ReturnValue obj -> Ok obj
    | Ok obj -> eval_program t obj env
    | Error msg -> Error msg

and eval_block statements evaluation env =
  match statements with
  | [] -> Ok (evaluation)
  | h :: t -> match eval (Statement h) env with
    | Ok Object.ReturnValue obj -> Ok (Object.ReturnValue obj)
    | Ok obj -> eval_block t obj env
    | Error msg -> Error msg

and wrap_statements = function
  | [] -> []
  | h :: t -> Statement h :: wrap_statements t

and eval_prefix_expr operator right =
  match operator with
  | Token.BANG -> eval_bang right
  | Token.MINUS -> eval_minus right
  | _ ->
    let* right = right in
    Error ("Unknown prefix operator: " ^ (Token.show_token operator) ^ (Object.show_obj right))

and eval_infix_expr operator left right =
  let* left = left in
  let* right = right in
  match operator, left, right with
  | _, Object.Integer left, Object.Integer right -> eval_integer_infix_expr operator left right
  | Token.EQ , Object.Boolean left, Object.Boolean right -> Ok (Object.Boolean (left = right))
  | Token.NOT_EQ, Object.Boolean left, Object.Boolean right -> Ok (Object.Boolean (left <> right))
  | _ -> Error ("Invalid infix expression: " ^ (Object.show_obj left) ^ " " ^ (Token.show_token operator) ^ " " ^ (Object.show_obj right))

and eval_integer_infix_expr operator left right =
  match operator with
  | Token.PLUS -> Ok (Object.Integer (left + right))
  | Token.MINUS -> Ok (Object.Integer (left - right))
  | Token.ASTERISK -> Ok (Object.Integer (left * right))
  | Token.SLASH -> Ok (Object.Integer (left / right))
  | Token.LT -> Ok (Object.Boolean (left < right))
  | Token.GT -> Ok (Object.Boolean (left > right))
  | Token.EQ -> Ok (Object.Boolean (left = right))
  | Token.NOT_EQ -> Ok (Object.Boolean (left <> right))
  | _ -> Error ("Unknown operator for integer infix expression: " ^ (Token.show_token operator))

and eval_if_expr condition consequence alternative env =
  let* condition = eval (Expression condition) env in

  match is_truthy condition with
  | true -> eval_block consequence Object.Null env
  | false -> match alternative with
    | [] -> Ok (Object.Null)
    | _ -> eval_block alternative Object.Null env

and is_truthy = function
  | Object.Null -> false
  | Object.Boolean true -> true
  | Object.Boolean false -> false
  | _ -> true

and eval_bang = function
  | Ok (Object.Boolean true) -> Ok (Object.Boolean (false))
  | Ok (Object.Boolean false) -> Ok (Object.Boolean (true))
  | Ok (Object.Null) -> Ok (Object.Boolean true)
  | _ -> Ok (Object.Boolean false)

and eval_minus right=
  match right with
  | Ok (Object.Integer integer) -> Ok (Object.Integer (-integer))
  | Ok (obj) -> Error ("Unknown - operator for " ^ (Object.show_obj obj))
  | Error msg -> Error msg