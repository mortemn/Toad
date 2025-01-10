let ( let* ) = Result.bind

let rec scanner env =
  print_string ">> ";
  let input = read_line () in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let* _, program = Parser.parse parser in
  match Evaluator.eval (Evaluator.Node program) env with
  | Ok obj -> print_endline (Object.to_string obj); scanner env
  | Error msg -> print_endline msg; scanner env