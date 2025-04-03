open Calc
open Parser
open Interpreter
open Utils
open Typechecker

let initialize_env () = Environment.StringMap.empty;;

let run_typechecker (ast : Ast.exp) =
  try
    (* Step 1: Perform type checking *)
    let _ = Utils.populate_symbol_table ast in
    let _ = Typechecker.type_check ast !symbol_table in
    
    (* Step 2: If we get here, type checking passed *)
    Printf.printf "Type checking passed successfully!\n";
    true
  
  with
  | TypeError error ->
      begin match error with
      | TypeMismatch (op, expected, actual) ->
          Printf.printf "Type error in %s: Expected %s, got %s\n"
            op
            (string_of_data_type expected)
            (string_of_data_type actual)
      | UndefinedVariable var ->
          Printf.printf "Type error: Undefined variable '%s'\n" var
      | IncompatibleDimensions (op, type1, type2) ->
          Printf.printf "Type error in %s: Incompatible dimensions %s and %s\n"
            op (string_of_data_type type1) (string_of_data_type type2)
      | InvalidOperation msg ->
          Printf.printf "Type error: Invalid operation - %s\n" msg
      | InvalidZeroDimension ->
          Printf.printf "Type Error: Vector/Matrix of dimension zero"
      | OtherError msg ->
          Printf.printf "Type error: %s\n" msg
      end;
      false
  | e ->
      Printf.printf "Unexpected error: %s\n" (Printexc.to_string e);
      false

let () =
let filename = 
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)  (* Use the first command-line argument *)
  else
    "testcases/input1.txt"  (* Default if no argument provided *)
  in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let env = initialize_env () in
  try
    let ast = Parser.top Calc.tokenize lexbuf in
    let passed_typechecker = run_typechecker ast in
    if not passed_typechecker then
      raise (Failure "Type checking failed. Exiting...")
    else
      let _ = Interpreter.eval env ast in
      close_in chan;
      ()
  with
  | Failure msg -> Printf.printf "Error: %s\n" msg
  | Parsing.Parse_error -> 
    let pos = Lexing.lexeme_start lexbuf in
    let error_char = Lexing.lexeme lexbuf in
    Printf.printf "Syntax error at position %d: Unexpected '%s'\n" pos error_char
  | _ -> Printf.printf "Unexpected error occurred.\n"
