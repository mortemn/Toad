let rec read_tokens lexer tokens =
        let new_lexer, token = Toad.Lexer.next_token lexer in
        match token with
        | Toad.Token.EOF -> tokens @ [Toad.Token.show_token token]
        | _ -> read_tokens new_lexer (tokens @ [Toad.Token.show_token token])

let _print_tokens lexer =
  let tokens = read_tokens lexer [] in
  List.iter (Printf.printf "%s\n") tokens

let lexer = Toad.Lexer.init "
let x = 2 * 3"

(* let () = print_tokens lexer *)

let parser = Toad.Parser.init lexer
let program = Toad.Parser.parse parser

let rec print_statement = function
  | [] -> ()
  | statement :: statements -> 
    Printf.printf "%s\n" (Toad.Ast.show_statement statement);
    print_statement statements

let () = match program with
  | Ok (_, program) -> Printf.printf "%s\n" (Toad.Ast.show_node program)
  | Error (_, msg, statements) -> 
    print_statement statements;
    Printf.printf "%s\n" msg;