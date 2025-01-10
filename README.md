# Toad
A minimal interpreter written in OCaml inspired by the book Writing an Interpreter in Go by Thorsten Ball.
## How to build
To run the REPL, make sure you have opam and dune installed. Then clone the project and run the commands

```
opam install . --deps-only
opam exec -- dune exec toad
```

## Description
This project includes a REPL which runs the interpreter and evaluates its inputs. As for the implementation, it includes the basic functionalities of the Monkey language, including conditionals, functions, and closures.
